--
-- Copyright (c) 2014 Citrix Systems, Inc.
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

{-# LANGUAGE PatternGuards #-}
module Vm.React
    ( monitorAndReactVm
    ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Monad.Error (catchError, throwError)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.IORef
import Data.String
import Data.Int
import System.IO
import Text.Printf (printf)
import Tools.Log
import Tools.XenStore
import Tools.Misc
import Tools.Text
import Tools.IfM
import Tools.Future
import System.Directory

import Vm.DomainCore (updateStubDomainID, domainXSPath, domainUIVM)
import Vm.Types
import Vm.Config
import Vm.Monitor
import Vm.Queries
import Vm.QueriesM
import Vm.Actions
import Vm.Monad
import Vm.State
import Rpc.Autogen.XenmgrNotify
import XenMgr.Expose.ObjectPaths
import XenMgr.Config
import XenMgr.Rpc
import XenMgr.Host
import XenMgr.HostOps
import XenMgr.PowerManagement
import XenMgr.XM
import XenMgr.CdLock

import qualified XenMgr.Connect.Xl as Xl
import Rpc.Autogen.NetworkDaemonClient
import Rpc.Autogen.CtxusbDaemonClient

usbdaemonService = "com.citrix.xenclient.usbdaemon"
usbUp   domid    = void . future $ comCitrixXenclientUsbdaemonNewVm usbdaemonService "/" domid
usbDown domid    = void . future $ comCitrixXenclientUsbdaemonVmStopped usbdaemonService "/" domid

-- | Fork monitor and start processing events
monitorAndReactVm :: Uuid -> XM ()
monitorAndReactVm uuid
    = do info $ "reactimating vm " ++ show uuid
         exist <- xmDoesVmContextExist uuid
         if exist
            then info $ "... already exists"
            else do
              xm_context <- xmContext
              monitor <- liftRpc $ newVmMonitor uuid
              c <- xmCreateAndRegisterVmContext uuid monitor
              processVmEvent <- vmEventProcessor
              liftRpc $ vmStateWatch monitor
              liftRpc . void $ react monitor (\hid e -> runVm c $ processVmEvent hid e)

-- TODO: clean method to stop monitoring events

-- | Vm event handler
newtype React
      = React [ VmEvent -> Vm () ]
mkReact f
  = React [\e -> f e]

ignore :: React
ignore = React []

whenE :: VmEvent -> Vm () -> React
whenE e f
  = mkReact $ \e' -> if (e == e') then f else return ()

instance Monoid React where
  mempty = ignore
  (React p) `mappend` (React q) = React (p ++ q)

data ShutdownReason = CreationFailure | Reboot | Hibernate | Halt | AcpiPoweroff
                    deriving (Eq, Show)

updateInternalStateR = mkReact f where
  f (VmStateChange state) =
    do uuid <- vmUuid
       liftRpc (updateVmInternalState uuid state)
  f _ = return ()

-- | default handlers
notifyExternalR = mkReact f where
    f (VmStateChange state)          = notifyVmStateChange state
    f (VmAcpiStateChange acpi_state) = notifyVmAcpiStateChange acpi_state
    f _ = return ()

runEventScriptR = mkReact f where
    -- ignore these three states as they coerce to same 'creating' state
    f (VmStateChange CreatingDomain) = return ()
    f (VmStateChange CreatingDevices) = return ()
    f (VmStateChange Created) = return ()
    f (VmStateChange state) =
      do uuid <- vmUuid
         liftRpc $ void $ runEventScript ContinueOnFail uuid getVmRunOnStateChange [uuidStr uuid, stateToPublicStr state]
    f (VmAcpiStateChange acpi_state) =
      do uuid <- vmUuid
         liftRpc $ void $ runEventScript ContinueOnFail uuid getVmRunOnAcpiStateChange [uuidStr uuid, show acpi_state]
    f _ = return ()

    catchScriptErrors run = run `catchError` reportErrors
    reportErrors e = warn (show e)

rpcAgentR =
            whenE VmRpcAgentStart (uuidRpc writeAgentAddonNodes)
  `mappend` whenE VmRpcAgentStop  (uuidRpc writeAgentAddonUninstallNodes)

detectPvAddonsR =
            whenE VmPvAddonsNodeChange checkPvAddons
  `mappend` whenE VmPvAddonsUninstallNodeChange checkPvAddonsUninstall

detectBsgDevStatusR =
  whenE VmBsgDevNodeChange checkBsgDevStatus

detectStateChange =
  whenE VmStateUpdate notifyVmStateUpdate

detectAcpiChange =
  whenE VmAcpiUpdate reactVmAcpiUpdate

clockR = mkReact f where
  f (VmRtcChange offset) = vmUuid >>= \uuid -> saveConfigProperty uuid vmTimeOffset offset
  f _ = return ()

measuresR xm_context = mkReact f where
  f (VmMeasurementFailure fpath expected actual)
    = do action <- liftRpc getMeasureFailAction
         let msg = printf "measuring of file %s FAILED. Expected checksum: %x actual %x. Performing %s" fpath expected actual (show action)
         liftIO (warn msg >> writeFile "/dev/xvc0" (msg++"\n"))
         runXM xm_context (executePmAction action)
  f _
    = return ()

bookkeepShutdownReasonR set_shr
  =           whenE (VmStateChange PreCreate) ( set_shr CreationFailure )
    `mappend` whenE (VmStateChange Created)   ( set_shr Halt )
    `mappend` whenE (VmStateChange Rebooted)  ( set_shr Reboot )
    `mappend` whenE (VmAcpiStateChange 4)     ( set_shr Hibernate )
    `mappend` whenE (VmAcpiStateChange 5)     ( set_shr AcpiPoweroff )

lifecycleR xm get_shr
  =
              whenE (VmStateChange Running)         ( whenRunning xm )
    `mappend` whenE (VmStateChange Shutdown)        ( whenShutdown xm =<< get_shr )
    `mappend` whenE (VmStateChange Rebooted)        ( whenRebooted xm )
    `mappend` whenE (VmAcpiStateChange 3)           ( whenAsleep )
    `mappend` whenE (VmAcpiStateChange 4)           ( whenHibernated )

-- | PowerLink functionality (i.e. change host state based on vm state)
powerlinkR xm get_shr =
            whenE (VmAcpiStateChange 4)    (plink hibernate)
  `mappend` whenE (VmAcpiStateChange 3)    (plink sleep)
  `mappend` whenE (VmStateChange Shutdown) (plink shutdown)

    where
      plink :: (VmContext -> XM ()) -> Vm ()
      plink f
        = do vm <- vmContext
             update <- liftRpc isUpdatePending
             when (not update) $
               whenControlsPlatformPState (vm_uuid vm) (runXM xm . hostWhenIdle $ f vm)

      hibernate vm     = do info "Power Link: hibernate"
                            liftRpc $ pmSetScreenRestoreVm (vm_uuid vm)
                            -- make sure the vm's hibernation is actually done as we receive acpi state change notifications
                            -- slightly before domain gets destroyed
                            done <- liftIO $ Xl.waitForState (vm_uuid vm) Shutdown (Just 60)
                            when (not done) $ warn ("Power Link: VM " ++ show (vm_uuid vm) ++ " is still running, but should be hibernated!")
                            hostHibernate
      sleep vm         = do info "Power Link: sleep"
                            liftRpc $ pmSetScreenRestoreVm (vm_uuid vm)
                            hostSleep
                            info $ "Power Link: resume " ++ show (vm_uuid vm)
                            liftIO (Xl.resumeFromSleep (vm_uuid vm))
                            return ()
      shutdown vm      = do reason <- runVm vm get_shr
                            when ( reason == AcpiPoweroff ) $
                              info "Power Link: shutdown" >> hostShutdown

logStatesR = mkReact f where
  f (VmStateChange s)
    = do uuid <- vmUuid
         info ("vm state change " ++ show uuid ++ ": " ++ stateToStr s)
  f (VmAcpiStateChange s)
    = do uuid <- vmUuid
         info ("vm acpi state change " ++ show uuid ++ ": " ++ show s)
  f _ = return ()

-- notifications sent to network daemon on networking backend domains start/stop
notifyNetworkDaemonR = mkReact f where
  f (VmStateChange Running)
    = do uuid <- vmUuid
         whenDomainID_ uuid $ \domid -> do
           whenM (getVmProvidesNetworkBackend uuid) $ liftRpc $
             comCitrixXenclientNetworkdaemonNdvmStatus "com.citrix.xenclient.networkdaemon" "/" (uuidStr uuid) (fromIntegral domid) eNDVM_STATUS_STARTED
  f (VmStateChange Shutdown)
    = do uuid <- vmUuid
         whenM (getVmProvidesNetworkBackend uuid) $ liftRpc $
             comCitrixXenclientNetworkdaemonNdvmStatus "com.citrix.xenclient.networkdaemon" "/" (uuidStr uuid) (-1) eNDVM_STATUS_STOPPED
  f _ = return ()

uuidRpc :: (Uuid -> Rpc a) -> Vm a
uuidRpc f = vmUuid >>= \uuid -> liftRpc (f uuid)

uuidIO :: (Uuid -> IO a) -> Vm a
uuidIO f = vmUuid >>= \uuid -> liftIO (f uuid)

vmEventProcessor :: XM (HandlerID -> VmEvent -> Vm ())
vmEventProcessor
    = do shut_r <- liftIO $ newMVar CreationFailure
         let set_shut_r reason = liftIO . void $ swapMVar shut_r reason
             get_shut_r        = liftIO $ readMVar shut_r
         xm <- xmContext
         let React r =
                         logStatesR
               `mappend` rpcAgentR
               `mappend` detectPvAddonsR
               `mappend` detectBsgDevStatusR
               `mappend` clockR
               `mappend` bookkeepShutdownReasonR set_shut_r
               `mappend` measuresR xm
               `mappend` notifyNetworkDaemonR
               `mappend` lifecycleR xm get_shut_r
               `mappend` updateInternalStateR
               `mappend` powerlinkR xm get_shut_r
               `mappend` runEventScriptR
               `mappend` notifyExternalR
               `mappend` detectStateChange
               `mappend` detectAcpiChange
         return $
                \hid e -> sequence_ $ [err (f e) | f <- r]
      where
        err f = f `catchError` report where report ex = warn ("reactor exception: " ++ show ex)
whenRunning xm = do
    maybeUpdateV4VHosts
    uuid <- vmUuid
    usb <- uuidRpc getVmUsbEnabled
    when usb $ whenDomainID_ uuid $ \domid -> usbUp (fromIntegral domid)

cleanupVkbd :: Uuid -> DomainID -> Rpc ()
cleanupVkbd uuid domid = do
    rpcCallOnce (Xl.xlInputDbus uuid "detach_vkbd" [toVariant $ (read (show domid) :: Int32) ])
    return ()

maybeCleanupSnapshots :: Vm ()
maybeCleanupSnapshots = do
    uuid <- vmUuid
    config <- liftRpc $ getVmConfig uuid False
    let disks = vmcfgDisks config
    info $ "cleanupSnapshots disks = " ++ (show disks)
    sequence $ map removeIfExists $ map (++".snap.tmp.vhd") $ map diskPath disks
    return ()
  where
    removeIfExists path = do
                            exists <- liftIO $ doesFileExist path
                            case exists of
                                True  -> do liftIO $ removeFile path
                                            return ()
                                False -> return ()

whenShutdown xm reason = do
    uuid <- vmUuid
    info ("vm " ++ show uuid ++ " shutdown, reason: " ++ show reason)
    domidStr <- liftIO $ xsRead ("/xenmgr/vms/" ++ show uuid ++ "/domid")
    case join (fmap maybeRead domidStr) of
      Just domid -> do
        usbDown domid
        removeAlsa domid
        cleanupV4VDevice domid
        vkb_enabled <- getVmVkbd uuid
        when vkb_enabled $ liftRpc $ cleanupVkbd uuid domid
      _ -> return ()
    liftIO $ removeVmEnvIso uuid
    uuidRpc disconnectFrontVifs
    -- has to be done before /xenmgr/vms/domid in XS is removed 
    uuidRpc unapplyVmFirewallRules `catchError` \err -> do
      warn $ "error while unapplying v4v firewall rules: " ++ show err
    uuidRpc exportVmSwitcherInfo
    maybeUpdateV4VHosts
    -- sent cd lock state notifications
    liftRpc $ mapM_ notifyCdDeviceAssignmentChanged =<< liftIO getHostBSGDevices
    maybeCleanupSnapshots
    runXM xm (maybeKeepVmAlive uuid)
    return ()
    where
      removeAlsa domid = liftIO $ do
        let alsafile = "/var/run/alsa-vm-" ++ show domid ++ ".conf"
        info $ "remove alsa file " ++ alsafile
        whenM (doesFileExist alsafile) (removeFile alsafile)

--Reboot has been slightly reworked. The domain is brought down by xl and
--restarted, XenMgr simply performs its regular duties on domain creation,
--synchronizing at the "Creating Devices" and "Created" states.
whenRebooted xm = do
    uuid <- vmUuid
    uuidRpc unapplyVmFirewallRules
    liftIO $ removeVmEnvIso uuid
    domidStr <- liftIO $ xsRead ("/xenmgr/vms/" ++ show uuid ++ "/domid")
    case join (fmap maybeRead domidStr) of
      Just domid -> do
        vkb_enabled <- getVmVkbd uuid
        when vkb_enabled $ liftRpc $ cleanupVkbd uuid domid
      _ -> return ()
    uuidRpc (backgroundRpc . runXM xm . startVm)
  where
    backgroundRpc f =
      do c <- rpcGetContext
         liftIO . void . forkIO $ (err =<< rpc c f)
        where
          err (Left e) = warn (show e)
          err _ = return ()

whenAsleep = do
    maybeWake
    return ()

whenHibernated =
    vmUuid >>= \uuid -> saveConfigProperty uuid vmHibernated True

whenControlsPlatformPState :: (MonadRpc e m) => Uuid -> m () -> m ()
whenControlsPlatformPState uuid act = getVmControlPlatformPowerState uuid >>= go where
    go True  = act
    go False = return ()

maybeWake :: Vm Bool
maybeWake = uuidRpc $ \uuid -> getVmAutoS3Wake uuid >>= wake uuid where
    wake uuid True = liftIO $ Xl.resumeFromSleep uuid >> return True
    wake _ _ = return False

maybeKeepVmAlive :: Uuid -> XM Bool
maybeKeepVmAlive uuid =
    do keep_alive <- liftRpc $ getVmKeepAlive uuid
       when keep_alive (startVm uuid)
       return keep_alive

-- Update /etc/hosts with v4v IPs for special domains, if setting says so
maybeUpdateV4VHosts :: Vm ()
maybeUpdateV4VHosts =
    do uuid <- vmUuid
       typ <- liftRpc (getVmType uuid)
       update_by_type typ
    where
      update_by_type (ServiceVm tag) = mupdate tag
      update_by_type _ = return ()

      mupdate tag = do
            uuid <- vmUuid
            updating <- liftRpc appV4VHostsFile
            name <- replace "\t" "-" . replace " " "-" <$> liftRpc (getVmName uuid)
            -- other processes are locking /etc/hosts, on vm startup, we have to hit proper time window
            when updating $ do
              updateEtcHosts name

updateEtcHosts :: String -> Vm ()
updateEtcHosts tag = uuidRpc $ \uuid -> do
  running  <- isRunning uuid
  domid    <- liftIO $ Xl.domainID uuid
  liftIO . withFile "/etc/hosts" ReadWriteMode $ \handle ->
      do str <- hGetContents' handle
         let contents = length str `seq` parse str
         hSetFileSize handle 0
         hSeek handle AbsoluteSeek 0
         hPutStrLn handle "127.0.0.1 localhost"
         hPutStr handle (unparse . process running domid $ contents)

  where
    parse = reverse . foldl parse_line [] . lines
    parse_line acc l | (ip:tag:stuff) <- words l, tag /= "localhost" = (ip,tag,stuff) : acc
                     | otherwise                 = acc
    unparse = unlines . map unparse_line
    unparse_line (ip,tag,stuff) = unwords (ip:tag:stuff)
    process True (Just domid) xs'
        = case process_set domid xs' of
            (True,xs) -> xs
            (False,xs) -> xs ++ [ (ip,tag,[]) ] where ip = "1.0.0." ++ show domid
    process _ _ xs'             = process_remove xs'

    process_remove [] = []
    process_remove (x@(ip,tag',stuff):xs) | tag' == tag = process_remove xs
                                          | otherwise   = x : process_remove xs
    process_set domid xs = foldl f (False,[]) xs
        where f (done,xs) x@(_,tag',stuff) | tag == tag' = (True, xs ++ [(ip,tag',stuff)])
                                           | otherwise   = (done, xs ++ [x])
              ip = "1.0.0." ++ show (domid :: DomainID)

hGetContents' h = aux h [] where
    aux h lines = do
      eof <- hIsEOF h
      if eof
         then return . unlines . reverse $ lines
         else do l <- hGetLine h
                 aux h (l:lines)

-- simulate xenstore writes when rpc agent start/stops
writeAgentAddonNodes :: Uuid -> Rpc ()
writeAgentAddonNodes uuid = whenDomainID_ uuid $ \domid ->
      do bi <- liftIO $ readBuildInfo
         case parse_tools_str (biTools bi) of
           Nothing -> return ()
           Just (major,minor,micro,build) -> liftIO $
               do xsWrite (pvAddonsPath domid ++ "/MajorVersion") major
                  xsWrite (pvAddonsPath domid ++ "/MinorVersion") minor
                  xsWrite (pvAddonsPath domid ++ "/MicroVersion") micro
                  xsWrite (pvAddonsPath domid ++ "/BuildVersion") build
                  xsWrite (pvAddonsPath domid ++ "/Installed") "1"
    where
      parse_tools_str s = case split '.' s of
                            [maj,min,mic,bui] -> Just (maj,min,mic,bui)
                            _ -> Nothing

writeAgentAddonUninstallNodes :: Uuid -> Rpc ()
writeAgentAddonUninstallNodes uuid = whenDomainID_ uuid $ \domid -> liftIO $
  do xsRm (pvAddonsPath domid)
     xsWrite (pvAddonsUninstallPath domid) "1"


pvAddonsPath :: DomainID -> String
pvAddonsPath domid = "/local/domain/" ++ show domid ++ "/attr/PVAddons"

pvAddonsUninstallPath :: DomainID -> String
pvAddonsUninstallPath domid = "/local/domain/" ++ show domid ++ "/attr/PVAddonsUninstalled"

checkPvAddons :: Vm ()
checkPvAddons = uuidRpc $ \uuid -> whenDomainID_ uuid $ \domid -> do
      info $ "testing pv addons installation for " ++ show uuid
      installedStr <- liftIO $ xsRead ( pvAddonsPath domid ++ "/Installed" )
      case installedStr of
        Just "1" -> do
          versions <-
                ( liftIO
                  . sequence
                  . map (\s -> xsRead ( pvAddonsPath domid ++ "/" ++ s ) )
                  $ [ "MajorVersion", "MinorVersion", "MicroVersion", "BuildVersion" ] )
          info $ "Detected PV drivers installation for " ++ show uuid
          saveConfigProperty uuid vmPvAddons True
          case filter isNothing versions of
            [] -> do let v = concat_versions $ map fromJust versions
                     saveConfigProperty uuid vmPvAddonsVersion v
                     info $ "PV drivers version for " ++ show uuid ++ " is " ++ v
            _  -> return () -- incomplete info
        _   ->
            return ()
    where
      concat_versions = concat . intersperse "."

checkPvAddonsUninstall :: Vm ()
checkPvAddonsUninstall = uuidRpc $ \uuid -> whenDomainID_ uuid $ \domid -> do
  info $ "testing pv addons un-installation for " ++ show uuid
  uninstalled  <- liftIO $ xsRead ( pvAddonsUninstallPath domid )
  case uninstalled of
    Just "1" -> do
      info $ "Detected PV drivers un-installation for " ++ show uuid
      saveConfigProperty uuid vmPvAddons False
      saveConfigProperty uuid vmPvAddonsVersion ""
    _ -> return ()

-- just send change message for each cd drive under the domain
checkBsgDevStatus :: Vm ()
checkBsgDevStatus = uuidRpc $ \uuid -> whenDomainID_ uuid $ \domid ->
  mapM_ notifyCdDeviceAssignmentChanged =<< liftIO (list domid) where
    list domid = catMaybes . map mkDev <$> xsDir ("/local/domain/" ++ show domid ++ "/bsgdev") where
      mkDev p = case split '_' p of
        [a,b,c,d] -> let r = maybeRead in
          BSGDevice <$> r a <*> r b <*> r c <*> r d
        _ -> Nothing

-- This is implemented in xenmgr for several reasons. First, there's really only
-- 1 acpi state we care about: s3. Xenvm used to fork a thread that polled xen
-- for domain acpi state with xc_hvm_param_get, which is BAD since xc_hvm_param_get
-- makes a hypercall.Second, we can remove responsibility from inputserver to react
-- to acpi state changes (it shouldn't have to deal with that anyway). Third, we 
-- won't have to patch libxl.
reactVmAcpiUpdate :: Vm ()
reactVmAcpiUpdate = do
    uuid <- vmUuid
    whenDomainID_ uuid $ \domid -> do
      maybe_acpi <- liftIO $ xsRead ("/local/domain/" ++ show domid ++ "/acpi-state")
      case maybe_acpi of
          Just acpi -> if acpi == "s3" then do switchVm domainUIVM 
                                               return () 
                                       else return () 
          Nothing   -> return () 
    
-- This is a new notify function to support state updates coming from xl
-- Instead of implementing dbus support in xl, state updates are written to a
-- xenstore node which XenMgr watches, which then fires off a dbus message, upon
-- which any state change code is handled normally.
notifyVmStateUpdate :: Vm ()
notifyVmStateUpdate = do
    uuid <- vmUuid
    maybe_state <- liftIO $ xsRead ("/state/" ++ show uuid ++ "/state")
    liftRpc $ notifyComCitrixXenclientXenmgrNotify
      xenmgrObjectPath
      (uuidStr uuid)
      (st maybe_state)
    where
    st s =
      case s of
        Just state -> (fromString "vm:state:" ++ state)
        Nothing -> (fromString "vm:state:shutdown")

notifyVmStateChange :: VmState -> Vm ()
notifyVmStateChange state
    = do uuid       <- vmUuid
         acpi_state <- liftRpc (getVmAcpiState uuid)
         notifyVmStatesChanged state acpi_state

notifyVmAcpiStateChange :: AcpiState -> Vm ()
notifyVmAcpiStateChange acpi_state
    = do state <- getVmState
         notifyVmStatesChanged state acpi_state

notifyVmStatesChanged :: VmState -> AcpiState -> Vm ()
notifyVmStatesChanged state acpi_state
    = do uuid <- vmUuid
         liftRpc $ notifyComCitrixXenclientXenmgrVmStateChanged
           xenmgrObjectPath
           (uuidStr uuid)
           (vmObjPath uuid)
           (stateToPublicStr state)
           (fromIntegral acpi_state)

