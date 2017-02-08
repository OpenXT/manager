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

{-# LANGUAGE ScopedTypeVariables,PatternGuards,TupleSections,ViewPatterns,FlexibleContexts #-}
module Vm.Actions
          ( configureServiceVm
          , startServiceVm
          , trashUnusedServiceVms
          , createVm, CreateVmPms(..), defaultCreateVmPms
          , removeVm
          , startVm
          , startVmInternal
          , rebootVm
          , sleepVm
          , resumeFromSleep
          , hibernateVm
          , shutdownVm
          , invokeShutdownVm
          , forceShutdownVm
          , invokeForceShutdownVm
          , pauseVm
          , unpauseVm
          , switchVm
          , reallySwitchVm
          , switchGraphicsFallback
          , bindVmPcis
          , loginToVm
          , addNicToVm, addDefaultNicToVm, removeNicFromVm
          , addDiskToVm, addDefaultDiskToVm, removeDiskFromVm, addVhdDiskToVm, addPhyDiskToVm
          , addVmFirewallRule, deleteVmFirewallRule, applyVmFirewallRules, unapplyVmFirewallRules
          , applyVmBackendShift
          , disconnectFrontVifs
          , createAndAddDiskToVm
          , createVhd
          , exportVmSwitcherInfo
          , modifyVmNic
          , modifyVmDisk
          , modifyVmPciPtRules
          , mountVmDisk
          , unmountVmDisk
          , generateCryptoKeyIn
          , generateCryptoKey
          , parallelVmExec
          , parallelVmExecByType
          , parallelVmExecInStages
          , suspendToFile, resumeFromFile
          , getMeasureFailAction, setMeasureFailAction
          , vmSuspendImageStatePath
          , runEventScript
          , changeVmNicNetwork
          , removeVmEnvIso
          -- property accessors
          , setVmType
          , setVmWiredNetwork, setVmWirelessNetwork, setVmGpu, setVmCd
          , setVmSeamlessTraffic
          , setVmStartOnBoot, setVmHiddenInSwitcher, setVmHiddenInUi, setVmMemory, setVmName
          , setVmImagePath, setVmSlot, setVmPvAddons, setVmPvAddonsVersion
          , setVmTimeOffset, setVmCryptoUser, setVmCryptoKeyDirs, setVmAutoS3Wake
          , setVmNotify, setVmHvm, setVmPae, setVmApic, setVmAcpi, setVmViridian, setVmNx, setVmSound, setVmDisplay
          , setVmBoot, setVmCmdLine, setVmKernel, setVmInitrd, setVmAcpiPath, setVmVcpus, setVmCoresPerSocket
          , setVmKernelExtract
          , setVmInitrdExtract
          , setVmMemoryStaticMax
          , setVmMemoryMin
          , setVmVideoram, setVmPassthroughMmio, setVmPassthroughIo, setVmFlaskLabel
          , setVmAmtPt, setVmHap, setVmSmbios, setVmDescription
          , setVmExtraXenvm, setVmExtraHvm
          , setVmStartOnBootPriority, setVmKeepAlive, setVmProvidesNetworkBackend
          , setVmProvidesGraphicsFallback, setVmShutdownPriority, setVmSeamlessId
          , setVmStartFromSuspendImage, setVmQemuDmPath, setVmQemuDmTimeout, setVmTrackDependencies
          , setVmSeamlessMouseLeft, setVmSeamlessMouseRight, setVmOs, setVmControlPlatformPowerState
          , setVmOemAcpiFeatures, setVmUsbEnabled, setVmUsbAutoPassthrough, setVmUsbControl, setVmCpuid
          , setVmStubdom, setVmStubdomMemory, setVmStubdomCmdline
          , setVmGreedyPcibackBind
          , setVmRunPostCreate, setVmRunPreDelete, setVmRunOnStateChange, setVmRunOnAcpiStateChange
          , setVmRunPreBoot
          , setVmRunInsteadofStart
          , setVmUsbGrabDevices
          , setVmNativeExperience, setVmShowSwitcher, setVmWirelessControl
          , setVmXciCpuidSignature
          , setVmS3Mode
          , setVmS4Mode
          , setVmVsnd
          , setVmRealm
          , setVmSyncUuid
          , setVmIcbinnPath
          , setVmOvfTransportIso
          , setVmDownloadProgress
          , setVmReady
          , setVmProvidesDefaultNetworkBackend
          , setVmVkbd
          , setVmVfb
          , setVmV4V
          , setVmRestrictDisplayDepth
          , setVmRestrictDisplayRes
          , setVmPreserveOnReboot
          , setVmBootSentinel
          , setVmHpet
          , setVmTimerMode
          , setVmNestedHvm
          , setVmSerial
          , setVmAutolockCdDrives
          , cleanupV4VDevice
          , EventHookFailMode(..)
          ) where

import Prelude hiding (catch, mapM, mapM_)
import Data.Char
import Data.List
import Data.Maybe
import Data.Bits
import Data.String
import Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Set as Set
import Text.Printf

import Control.Arrow
import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as E

import System.FilePath.Posix
import System.Posix.Files
import System.Process
import System.Exit
import System.IO
import System.Timeout
import Data.Time
import Directory
import System.Directory (createDirectoryIfMissing)
import qualified Data.Foldable

import Tools.Log
import Tools.XenStore
import Tools.Misc
import Tools.Process
import Tools.Text
import Tools.IfM
import Tools.FreezeIOM
import Tools.Future
import Tools.File (fileSha1Sum)
import Tools.Apptool
import Vm.Types
import Vm.Config
import Vm.ConfigWriter
import Vm.Dm
import Vm.Queries
import Vm.QueriesM
import Vm.Pci
import Vm.Utility
import Vm.Policies
import Vm.Templates
import Vm.Monad
import Vm.Monitor
import Vm.State
import Vm.DomainCore
import {-# SOURCE #-} Vm.React
import qualified Vm.V4VFirewall as Firewall
import Vm.Balloon
import XenMgr.Rpc
import qualified XenMgr.Connect.Xl as Xl
import qualified XenMgr.Connect.GuestRpcAgent as RpcAgent
import XenMgr.Connect.Xl ( resumeFromSleep, resumeFromFile, suspendToFile )
import XenMgr.Connect.NetworkDaemon
import XenMgr.Connect.InputDaemon
import XenMgr.Config
import XenMgr.Errors
import XenMgr.Db
import XenMgr.Host
import XenMgr.Diskmgr
import XenMgr.Notify
import XenMgr.XM
import XenMgr.CdLock
import {-# SOURCE #-} XenMgr.PowerManagement
import Rpc.Autogen.XenmgrNotify
import XenMgr.Expose.ObjectPaths
import Vm.Pci

data EventHook
   = EventScript FilePath
   | EventRpc (Maybe Uuid) RpcCall
     deriving (Show)

data EventHookFailMode
   = HardFail
   | ContinueOnFail
     deriving (Eq, Show)

vmSuspendImageStatePath :: Uuid -> String
vmSuspendImageStatePath uuid = "/xenmgr/service-vm-snapshot/" ++ show uuid ++ "/state"

startServiceVm :: Uuid -> XM ()
startServiceVm uuid = xmContext >>= \xm -> liftRpc $
    isRunning uuid >>= \r ->
        case r of
          False -> do info $ "starting service vm " ++ show uuid
                      file <- getVmStartFromSuspendImage uuid
                      context <- rpcGetContext
                      if (not . null $ file)
                         then liftIO $ do
                           xsWrite (vmSuspendImageStatePath uuid) "start"
                           void . forkIO . xsWaitFor (vmSuspendImageStatePath uuid) $
                                  do status <- (rpc context $ snapshot_request xm file)
                                     case status of
                                       Right rv -> return rv
                                       Left err -> warn (show err) >> return True
                         else liftIO $ do
                           xsWrite (vmSuspendImageStatePath uuid) "start-no-snapshot"

                      runXM xm (startVm uuid)
          True  -> info $ "service vm " ++ show uuid ++ " already running"
    where
      snapshot_request xm file =
          do state <- liftIO $ xsRead (vmSuspendImageStatePath uuid)
             debug $ "service vm " ++ show uuid ++ " suspend image state = " ++ show state
             if state /= Just "snapshot"
                then return False -- continue waiting
                else do
                  info $ "service vm " ++ show uuid ++ " requested a memory image snapshot"
                  -- take a vm snapshot
                  info $ "taking memory image snapshot for service vm " ++ show uuid
                  liftIO $ Xl.suspendToFile uuid file
                  info $ "DONE taking memory image snapshot for service vm " ++ show uuid
                  liftIO $ xsWrite (vmSuspendImageStatePath uuid) "snapshot-done"
                  -- double start, TODO: maybe wont be necessary
                  runXM xm (startVm uuid)
                  -- finished waiting on this watch
                  return True

parseEventHooks :: String -> [EventHook]
parseEventHooks = catMaybes . map parseEventHook . split ';'

parseEventHook :: String -> Maybe EventHook
parseEventHook s = parse s where
  parse "" = Nothing
  parse s | "rpc:" `isPrefixOf` s = rpc (drop 4 s) -- assume rpc call
  parse s = return (EventScript s) -- assume script file
  
  rpc s = do
    let kv      = keyvals (filter (not . isSpace) s)
        objpath = fromMaybe "/" ("objpath" `M.lookup` kv)
        vm      = fromString <$> "vm" `M.lookup` kv
        
    dest <- "destination" `M.lookup` kv
    interface <- "interface" `M.lookup` kv
    member <- "member" `M.lookup` kv
    return . EventRpc vm $ RpcCall (fromString dest) (fromString objpath) (fromString interface) (fromString member) []

  keyvals = M.fromList . catMaybes . map one . split ',' where
    one x = case split '=' x of
      [k,v] -> Just (k,dequote v)
      _ -> Nothing
  dequote (x:xs) | is_quote x = reverse (dequote $ reverse xs) where is_quote x = x == '\'' || x =='"'
  dequote xs = xs

runEventScript :: EventHookFailMode -> Uuid -> (Uuid -> Rpc (Maybe String)) -> [String] -> Rpc Bool
runEventScript failmode vm get_hooks args =
  run_hooks =<< get_hooks vm where
    quote x = "\"" ++ x ++ "\""
    run_hooks Nothing    = return False
    run_hooks (Just str) = (catchScriptErrors (mapM_ run_hook . parseEventHooks $ str)) >> return True
    run_hook (EventScript file) = liftIO $ run_file file
    run_hook (EventRpc vm call) = void $ run_rpc vm (setCallArgs call args)
    run_rpc vm rpc =
      do info $ "event-rpc " ++ show rpc
         case vm of
           Nothing   -> rpcCall rpc
           Just uuid -> do
             domid <- getDomainID uuid
             let fail       = error $ "required VM " ++ show uuid ++ " is not running"
                 call domid = rpcWithDomain (fromIntegral domid) (rpcCall rpc)
             maybe fail call domid

    run_file path =
      do info $ "event-script " ++ show path ++ " " ++ (intercalate " ") (map quote args)
         exec path
         return ()
    exec path =
      -- stderr redirected to warn, stdout to info
      runInteractiveProcess path args Nothing Nothing >>= \ (_, stdout, stderr, h) ->
          do hSetBuffering stdout NoBuffering
             hSetBuffering stderr NoBuffering
             contents_mv <- newEmptyMVar
             forkIO $ consume_lines stderr warn >> return ()
             forkIO $ putMVar contents_mv =<< consume_lines stdout info
             contents <- takeMVar contents_mv
             -- force evaluation of contents
             exitCode <- contents `seq` waitForProcess h
             case exitCode of
               ExitSuccess -> return $ contents
               _           -> error  $ "event-script: " ++ path ++ " FAILED."
    consume_lines h feed = go h [] where
          go h ls = continue =<< E.try (hGetLine h)
              where continue :: Either E.SomeException String -> IO String
                    continue (Right l) = feed l >> go h (l:ls)
                    continue (Left _)  = return . unlines . reverse $ ls
    catchScriptErrors run = run `catchError` reportErrors
    reportErrors e = warn (show e) >> when (failmode == HardFail) (throwError e)
    setCallArgs c args = c { callArgs = map toVariant args }


configureServiceVm :: String -> Rpc (Maybe Uuid)
configureServiceVm tag =
    do overwrite_settings <- appOverwriteServiceVmSettings tag
       template <- liftIO $ getServiceVmTemplate tag
       case getUuidInTemplate template of
         Nothing -> error "service vm template does not have uuid"
         Just uuid ->
             do exists <- dbExists $ "/vm/" ++ show uuid
                when ( overwrite_settings || not exists ) $ exportTemplate Nothing template >> return ()
                info $ "configured service vm " ++ show tag
                return . Just $ uuid

-- remove service vms which are not in active templates from db
trashUnusedServiceVms :: Rpc ()
trashUnusedServiceVms =
    do tags <- liftIO enumServiceVmTags
       mapM_ (trash tags) =<< getVms
       where
         trash active uuid =
             getVmType uuid >>= trash_type where
                 trash_type (ServiceVm tag) | not (tag `elem` active) = dbRm $ "/vm/" ++ show uuid
                 trash_type _                                         = return ()

-- Returns first empty slot in 1..9 range, or Nothing if all are in use
findEmptySlot :: Rpc (Maybe Int)
findEmptySlot =
    fmap (first . inverse) . getSlots =<< getGuestVms
  where
    getSlots      = mapM getSlot
    getSlot uuid  = fromMaybe 0 <$> readConfigProperty uuid vmSlot
    inverse slots = [1..9] \\ slots
    first = listToMaybe . sort

-- Create a VM
data CreateVmPms
   = CreateVmPms { cvmUuid :: Maybe Uuid
                 , cvmTemplate :: Maybe String
                 , cvmExtraJson :: String
                 , cvmName :: Maybe String
                 , cvmDescription :: Maybe String
                 , cvmImagePath :: Maybe String
                 , cvmAutoSlot :: Bool }

defaultCreateVmPms :: CreateVmPms
defaultCreateVmPms
    = CreateVmPms
        Nothing
        Nothing
        ""
        Nothing
        Nothing
        Nothing
        True

-- | bind passthrough devices to pciback
-- this typically might be done early (on daemon start), to prevent the devices going into
-- use by other, non-passthrough vms
bindVmPcis :: Uuid -> Rpc ()
bindVmPcis uuid = whenM (getVmGreedyPcibackBind uuid)
                  (mapM_ bind =<< filter (isconfig . pciPtSource) <$> getPciPtDevices uuid)
  where
    bind = liftIO . pciBindPciback . devAddr . pciPtDevice
    isconfig SourceConfig = True
    isconfig _ = False

createVm :: Bool -> CreateVmPms -> XM Uuid
createVm unrestricted pms = do
    unlessM (liftM2 (||) (return unrestricted) policyQueryVmCreation) failActionSuppressedByPolicy
    info "creating new VM..."
    -- creation needs a lock around otherwise it produces non unique slots now and then
    xmWithVmCreationLock do_create

  where
    do_create = do
      let extra = templateFromString (cvmExtraJson pms)
      template <- mergeTemplates <$> (
        case cvmTemplate pms of
                    Nothing -> liftIO getNewVmTemplate
                    Just s | null (strip s) -> return nullTemplate
                    Just n  -> liftIO $ getAnyVmTemplate n
        ) <*> pure extra
      let auto_allocate_slot =
            cvmAutoSlot pms && isNothing (getSlotInTemplate template)
      maybeSlot <- liftRpc findEmptySlot
      when (auto_allocate_slot && isNothing maybeSlot) $ failTooManyVms
      uuid <- case catMaybes [ cvmUuid pms, getUuidInTemplate template ] of
        []    -> liftIO uuidGen
        (u:_) -> return u
      info $ "exporting vm template for " ++ show uuid
      liftRpc (exportTemplate (Just uuid) template)
      info $ "setting initial properties for " ++ show uuid
      -- Give it seamless ID, equal to uuid from creation
      setVmSeamlessId uuid (show uuid)
      -- Give it name and slot
      whenM (null <$> liftRpc (getVmName uuid)) $
        saveConfigProperty uuid vmName $
          fromMaybe ("name-" ++ show uuid) (cvmName pms)
      when auto_allocate_slot $
           let Just n = maybeSlot in
           saveConfigProperty uuid vmSlot n
      -- other optional settings
      maybeM (cvmDescription pms) $ saveConfigProperty uuid vmDescription
      maybeM (cvmImagePath   pms) $ saveConfigProperty uuid vmImagePath

      -- Begin monitoring
      monitorAndReactVm uuid

      return uuid

    maybeM x f = maybe (return ()) f x

-- Remove a VM
removeVm :: Uuid -> Rpc ()
removeVm uuid =
    do -- not allowing to remove running VMS
       whenM (isRunning uuid) failCannotRemoveRunning
       info $ "Removing a VM " ++ show uuid

       runEventScript HardFail uuid getVmRunPreDelete [uuidStr uuid]

       -- Delete VHDS, but only if this is not a managed VM
       unlessM (isManagedVm uuid) removeVhds
       -- remove ovffiles
       liftIO $ readProcessOrDie "rm" ["-rf", "/storage/ovffiles/" ++ show uuid] ""
       -- Away from database
       dbRm $ "/vm/" ++ show uuid
       dbRm $ "/dom-store/" ++ show uuid
       
       -- Need to quit xenvm
       -- FIXME: cleanly stop monitoring events
       removeDefaultEvents uuid	--cleanly...stop monitoring events
       notifyVmDeleted uuid
  where
    removeVhds  = Data.Foldable.mapM_ (removeDiskFiles uuid) =<< getDisks uuid
    removeVhd d
      | not (diskShared d), diskType d == VirtualHardDisk
      = do refs <- nub . map fst <$> getVhdReferences (diskPath d)
           -- only remove when only this vm references the vhd
           when (refs == [uuid]) $ do
             let p = diskPath d
                 b | ".vhd" `isSuffixOf` p = take (length p - 4) p
                   | otherwise = p
                 snappaths = map (b ++) [".vhd.hib", ".snap.tmp.vhd", ".snap"]
             liftIO . whenM (doesFileExist p) $ do
               info $ "Removing VHD file " ++ p
               removeLink p
             let rmSnapshot p = liftIO . whenM (doesFileExist p) $ do
                   info $ "Removing snapshot file " ++ p
                   removeLink p
             mapM_ rmSnapshot $ snappaths
    removeVhd _ = return ()

-- which vms are referencing this vhd
-- TODO: handle differencing disks
getVhdReferences :: FilePath -> Rpc [(Uuid, Disk)]
getVhdReferences vhd = concat <$> (mapM (diskVhdReferences vhd) =<< getVms) where
  diskVhdReferences vhd vm = zip (repeat vm) . filter (references vhd) . M.elems <$> getDisks vm where
    references vhd disk = diskPath disk == vhd

startVm :: Uuid -> XM ()
startVm uuid = do
  withPreCreationState uuid $ do
    ran <- liftRpc $ runEventScript HardFail uuid getVmRunInsteadofStart [uuidStr uuid]
    when (not ran) $ startVmInternal uuid

--Add a passthrough rule to vm config
add_pt_rule_bdf uuid dev = modifyVmPciPtRules uuid $ pciAddRule (form_rule_bdf (show (devAddr dev)))

form_rule_bdf = rule . fromMaybe (error "error parsing rule") . pciAndSlotFromStr where
  rule (addr,sl) = PciPtRuleBDF addr sl

-- Start a VM! (maybe, because stuff can happen not)
startVmInternal :: Uuid -> XM ()
startVmInternal uuid = do
    unlessM (dbExists $ "/vm/" ++ show uuid) $ error ("vm does not have a database entry: " ++ show uuid)
    info $ "starting VM " ++ show uuid
    liftRpc $ maybePtGpuFuncs uuid
    config <- prepareAndCheckConfig uuid
    case config of
      Just c -> info ("done checks for VM " ++ show uuid) >> bootVm c
      Nothing-> return ()
  where

  --Based on bdf get all functions on that device bus:device.function
  --and pass them through to vm in case of bus level reset.
    maybePtGpuFuncs uuid = do
      ok <- isGpuPt uuid
      if ok
        then do
          gfxbdf <- getVmGpu uuid
          devices <- liftIO pciGetDevices
          let devMatches = filter (bdFilter (take 7 gfxbdf) gfxbdf) devices in
              mapM_ (add_pt_rule_bdf uuid) devMatches
        else return ()

    --Filter function to match on domain:bus and also filter out the video function
    bdFilter match bdf d = (isInfixOf match (show (devAddr d))) && (bdf /= (show (devAddr d))) 

    --Check if vm has a bdf in gpu
    isGpuPt uuid = do
        gpu <- getVmGpu uuid
        return (gpu /= "" && gpu /= "hdx")

    prepareAndCheckConfig uuid = do
      ok <- stage1 -- early tests / dependency startup
      if (not ok)
         then return Nothing
         else do
           -- this (config gather) needs to be done after dependency startup to get correct
           -- backend domids
           config <- liftRpc $ getVmConfig uuid True
           info $ "gathered config for VM " ++ show uuid
           ok <- stage2 config
           if ok then return (Just config) else return Nothing

    stage1
      = startupCheckVmState uuid
          `followby` startupCheckHostStates uuid
          `followby` startupDependencies uuid
          `followby` startupExtractKernel uuid
          `followby` startupMeasureVm uuid
    stage2 config
      = startupCheckNics config
          `followby` startupCheckGraphicsConstraints config
          `followby` startupCheckIntelConstraints config
          `followby` startupCheckAMTConstraints config
          `followby` startupCheckPCI config
          `followby` startupCheckOemFeatures config
          `followby` startupCheckSyncXTComfortable uuid

    followby f g =
      do ok <- f
         if ok then g else return False

startupCheckVmState :: Uuid -> XM Bool
startupCheckVmState uuid
  = do running <- liftRpc (isRunning uuid)
       if running
         then do warn ("request to start vm " ++ show uuid ++ " BUT it is already running")
                 return False
         else do return True

startupCheckHostStates :: Uuid -> XM Bool
startupCheckHostStates uuid
  = f =<< (,) <$> liftRpc getHostState <*> liftIO getCurrentRunLevel
    where
      f (_, Just 0) = badRunLevel 0
      f (_, Just 6) = badRunLevel 6
      f (s, _) | s /= HostIdle = badHostState s
      f _ = return True

      msg reason     = warn ("ignoring request to start VM " ++ show uuid ++ " because of " ++ reason)
      badRunLevel  l = msg ("current runlevel: " ++ show l) >> return False
      badHostState s = msg ("current host state: " ++ show s) >> return False

startupMeasureVm :: Uuid -> XM Bool
startupMeasureVm uuid
  = do measure <-  getVmMeasured uuid
       if not measure
         then return True
         else xmRunVm uuid $ addVmDiskHashes >> checkVmDiskHashes

startupDependencies :: Uuid -> XM Bool
startupDependencies uuid
  = do deps <- liftRpc $ getVmTrackDependencies uuid
       if deps
         then do
           startDependencies uuid
           checkDependencies uuid
         else do
           return True
  where
    checkDependencies uuid = liftRpc $ do
        missing <- filterM (fmap not . isRunning) =<< (getVmDependencies uuid)
        if (null missing)
            then do return True
            else do warn $ "missing dependencies: " ++ show missing
                    return False

    startDependencies uuid =
        do dependentVms <- liftRpc $ getVmDependencies uuid
           unless (null dependentVms) $
                info $ "vm dependencies: " ++ show dependentVms
           mapM_ startVm =<< (liftRpc $ filterM (fmap not . isRunning) dependentVms)

startupExtractKernel :: Uuid -> XM Bool
startupExtractKernel uuid
  = do liftRpc $ extractKernelFromPvDomain uuid
       liftRpc $ extractInitrdFromPvDomain uuid
       return True

startupCheckNics :: VmConfig -> XM Bool
startupCheckNics cfg
  = mapM_ verify (vmcfgNics cfg) >> return True
    where
      verify nic
        | nicdefBackendDomid nic == Nothing && (nicdefBackendUuid nic /= Nothing || nicdefBackendName nic /= Nothing)
          = failNetworkDomainNotRunning
        | otherwise
          = return ()

startupCheckGraphicsConstraints :: VmConfig -> XM Bool
startupCheckGraphicsConstraints cfg
  | (vmcfgGraphics cfg == HDX)
    = hdxCount >> vtd >> return True

  | otherwise
    = return True

  where
    hdxCount = liftRpc $ do
      hdx <- getRunningHDX
      case vmcfgVgpuMode cfg of
        Just vgpu | vgpuMaxVGpus vgpu < length hdx + 1 -> failCannotStartBecauseHdxRunning
        _ -> return ()

    vtd = do
      hvmInfo <- liftIO getHvmInfo
      let vtd = hvmDirectIOEnabled hvmInfo
      when (not vtd)$ failCannotStartHdxWithoutVtD

startupCheckAMTConstraints :: VmConfig -> XM Bool
startupCheckAMTConstraints cfg
  = do whenM (liftRpc $ getVmAmtPt $ vmcfgUuid cfg) $ unique
       return True
    where
      unique = liftRpc $
        getGuestVms >>= filterM isRunning >>= filterM getVmAmtPt >>= maybe_fail
          where maybe_fail [] = return ()
                maybe_fail (uuid:_) = failCannotStartBecauseAmtPtRunning

startupCheckOemFeatures :: VmConfig -> XM Bool
startupCheckOemFeatures config = liftRpc $ do
    let features = vmcfgOemAcpiFeatures config
    when features $
      getGuestVms >>= filterM isRunning >>= filterM getVmOemAcpiFeatures >>= maybe_fail
    return True
 where
   maybe_fail [] = return ()
   maybe_fail (uuid:_) = failCannotStartBecauseOemFeaturesRunning

startupCheckPCI :: VmConfig -> XM Bool
startupCheckPCI cfg = liftRpc $
    do in_use <- map pciPtDevice <$> online_devices
       case vm_devices `intersect` in_use of
         []      -> return True
         (dev:_) -> failDeviceAlreadyPassedThrough (show dev)
    where
      vm_devices
        = map pciPtDevice (vmcfgPciPtDevices cfg)
      online_devices
        = concat <$> (getGuestVms >>= filterM isRunning >>= mapM getPciPtDevices)

startupCheckIntelConstraints :: VmConfig -> XM Bool
startupCheckIntelConstraints cfg
  = do mapM_ verify devs
       return True
    where
      devs = vmcfgPciPtDevices cfg
      verify (PciPtDev { pciPtDevice=d }) = liftRpc $
              do info <- liftIO $ pciGetInfo d
                 case info of
                   Nothing   -> return ()
                   Just info -> do
                       -- intel gpu cannot be passed as secondary adapter
                       case (pciinfoVendor info,pciinfoClass info) of
                         (0x8086,0x300) -> check d
                         (0x8086,0x380) -> check d
                         _ -> return ()

              where
                check d =
                    do let boot_vga_file = devSysfsPath d </> "boot_vga"
                       boot_vga_exists <- liftIO $ doesFileExist boot_vga_file
                       when boot_vga_exists $
                            do contents <- chomp  <$> (liftIO $ readFile boot_vga_file)
                               case contents of
                                 "0" -> failGraphicsCantBePassedAsSecondaryAdapter
                                 _   -> return ()

startupCheckSyncXTComfortable :: Uuid -> XM Bool
startupCheckSyncXTComfortable uuid
  = do bg_op_ok <- liftRpc (getVmReady uuid)
       -- FIXME: perform this check only for syncxt vms if still needed
       -- when (not bg_op_ok) $ failVmNotReady
       return True

withPreCreationState :: Uuid -> XM a -> XM a
withPreCreationState uuid f =
  do
     f `catchError` (\e -> do
                               s <- getVmInternalState uuid
                               -- have to mop up here if something went wrong in pre-create state
                               -- since xenvm does not know about this state
                               when (s == PreCreate) $ do
                                 xmRunVm uuid $ vmEvalEvent (VmStateChange Shutdown)
                               throwError e)

--Write the xenstore nodes for the backend and the frontend for the v4v device
--set states to Unknown and Initializing respectively, like xenvm used to do
xsp domid = "/local/domain/" ++ show domid
xsp_dom0  = "/local/domain/0"
v4vBack domid = "/backend/v4v/" ++ show domid ++ "/0"
vfbBack domid = "/backend/vfb/" ++ show domid ++ "/0"

setupV4VDevice uuid =
  whenDomainID_ uuid $ \domid -> liftIO $ do
    xsWrite (xsp domid ++ "/device/v4v/0/backend") ("/local/domain/0/backend/v4v/" ++ show domid ++ "/0")
    xsWrite (xsp domid ++ "/device/v4v/0/backend-id") "0"
    xsWrite (xsp domid ++ "/device/v4v/0/state") "1"
    xsChmod (xsp domid ++ "/device/v4v/0/backend") ("n"++show domid++",r0")
    xsChmod (xsp domid ++ "/device/v4v/0/backend-id") ("n"++show domid++",r0")
    xsChmod (xsp domid ++ "/device/v4v/0/state") ("n"++show domid++",r0")


    xsWrite (xsp_dom0 ++ (v4vBack domid) ++ "/frontend") (xsp domid ++ "/device/v4v/0")
    xsWrite (xsp_dom0 ++ (v4vBack domid) ++ "/frontend-id") $ show domid
    xsWrite (xsp_dom0 ++ (v4vBack domid) ++ "/state") "0"

cleanupV4VDevice domid = liftIO $ do
    xsRm (xsp_dom0 ++ "/backend/v4v/" ++ show domid)
   
setupAcpiNode uuid = 
  whenDomainID_ uuid $ \domid -> do
     stubdom <- getStubDomainID uuid
     liftIO $ xsWrite (xsp domid ++ "/acpi-state") ("")
     case stubdom of
         Just stubdomid -> liftIO $ xsChmod (xsp domid ++ "/acpi-state") ("b" ++ show stubdomid)
         Nothing        -> liftIO $ xsChmod (xsp domid ++ "/acpi-state") ("b" ++ show domid)
 
--Watch acpi state when booting a VM, used to be handled in xenvm
monitorAcpi :: Uuid -> VmMonitor -> AcpiState -> IO ()
monitorAcpi uuid m state = do
    acpi_state <- Xl.acpiState uuid
    if state == 5
      then do return ()
      else do
        if state /= acpi_state
          then do vmStateSubmit m
                  threadDelay (10^6)
                  monitorAcpi uuid m acpi_state
          else do threadDelay (10^6)
                  monitorAcpi uuid m acpi_state

-- Creates a snapshot from the primary disk when the disk persistence option is
-- set. It first checks to see if a snapshot already exists and deletes it before
-- creating a new one.
createSnapshot :: Disk -> IO Disk
createSnapshot disk = do
    let path = diskPath disk
    let newPath = path ++ ".snap.tmp.vhd"
    exists <- liftIO $ doesFileExist newPath --ensure we're creating a fresh snapshot
    when exists (removeFile newPath)
    create path newPath
  where
    create path newPath = do readProcess "vhd-util" ["snapshot", "-n", newPath, "-p", path] []
                             info $ "newPath = " ++ newPath
                             return disk { diskPath = newPath }

checkAndPerformSnapshotIfReq :: Uuid -> [Disk] -> IO [Disk]
checkAndPerformSnapshotIfReq uuid disks = do
    mapM checkDisk disks

  where
    checkDisk disk = do
        let snapshot = diskSnapshotMode disk
        case snapshot of
            Nothing                          -> return disk --return the same disk we were passed in, no changes required
            Just SnapshotTemporary           -> createSnapshot disk
            _                                -> return disk --other Snapshot types unimplemented for now since UI can't set them


bootVm :: VmConfig -> XM ()
bootVm config
  = do
       monitor <- vm_monitor <$> xmRunVm uuid vmContext

       -- Check persistence type, create snapshot and update path if required
       newDisks <- liftIO $ checkAndPerformSnapshotIfReq uuid (vmcfgDisks config)
       let newConfig = config { vmcfgDisks = newDisks }

       liftRpc $ updateXVConfig newConfig

       withPreCreationState uuid (create monitor)
    where
      uuid = vmcfgUuid config
      updateXVConfig :: VmConfig -> Rpc ()
      updateXVConfig c = writeXlConfig c
      create monitor = do
       -- create environment iso
       whenM (getVmOvfTransportIso uuid) . liftIO $ do
         createDirectoryIfMissing True envIsoDir
         generateEnvIso uuid (envIsoPath uuid)
         info $ "generated ovf environment ISO " ++ (envIsoPath uuid) ++ " for VM " ++ show uuid
       -- try some ballooning if we lack memory
       -- fork and monitor Acpi state
       bootstrap <- do

         -- Clear the hibernated property
         saveConfigProperty uuid vmHibernated False

         -- run custom pre boot action
         liftRpc $ runEventScript HardFail uuid getVmRunPreBoot [uuidStr uuid]
         -- bind any passthrough devices to pciback if possible
         liftRpc $ bindVmPcis uuid

         -- fork xenvm vm startup in the background
         bootstrap <- future $ liftRpc $ do
           suspend_file <- getVmStartFromSuspendImage uuid
           exists <-
             if null suspend_file
                  then return False
                  else liftIO (doesFileExist suspend_file)
           if not exists
                then do liftIO $ Xl.start uuid --we start paused by default
                else do liftIO $ xsWrite (vmSuspendImageStatePath uuid) "resume"
                        liftIO $ Xl.resumeFromFile uuid suspend_file False True
         return bootstrap
       -- fork vm creation phase handling in the background
       phases <- future handleCreationPhases
       -- ensure bootstrap and phase handling synchronously terminates before returning (and errors get propagated)
       force bootstrap
       force phases
       liftIO . void $ forkIO $ monitorAcpi uuid monitor 0

      writable domid path = do
        xsWrite path ""
        xsSetPermissions path [ Permission 0 []
                              , Permission (fromIntegral domid) [PermRead,PermWrite]]

      setupCDDrives :: Uuid -> Rpc ()
      setupCDDrives uuid = do
          -- use cd autolocking perhaps
          setVmAutolockCdDrives uuid =<< appGetAutolockCdDrives
          -- make bsg device status & req nodes writable by domain
          whenDomainID_ uuid $ \domid -> liftIO $ do
            writable domid (xsp domid ++ "/bsgdev")
            writable domid (xsp domid ++ "/bsgdev-req")
          -- read drives media state
          liftIO $
            mapM_ updateCdDeviceMediaStatusKey =<< liftIO getHostBSGDevices

      --setupBiosStrings uuid =
      --    whenDomainID_ uuid $ \domid -> do
      --      liftIO $ xsWrite (xsp domid ++ "/bios-strings/xenvendor-manufacturer") "OpenXT"
      --      liftIO $ xsWrite (xsp domid ++ "/bios-strings/xenvendor-product") "OpenXT 5.0.0"
       --     liftIO $ xsWrite (xsp domid ++ "/bios-strings/xenvendor-seamless-hint") "0"

      --Changes to surfman/inputserver moved these calls into xenvm. With xenvm removal, it's easier to call
      --them from Xenmgr.
      surfmanDbusCalls uuid =
          whenDomainID_ uuid $ \domid -> do
            rpcCallOnce (Xl.xlSurfmanDbus uuid "set_pv_display" [toVariant $ (read (show domid) :: Int32), toVariant $ ""])
            rpcCallOnce (Xl.xlSurfmanDbus uuid "set_visible" [toVariant $ (read (show domid) :: Int32), toVariant $ (0 :: Int32), toVariant $ False])
            return ()

      inputDbusCalls uuid =
          whenDomainID_ uuid $ \domid -> do
            rpcCallOnce (Xl.xlInputDbus uuid "attach_vkbd" [toVariant $ (read (show domid):: Int32) ])
            return ()

      handleCreationPhases :: XM ()
      handleCreationPhases = do
        waitForVmInternalState uuid CreatingDevices 30

        --Move these tasks up earlier in the guest boot process. Prevents the need
        --for XL to implement a handshake with xenmgr for v4v firewall rules. Also speeds up
        --the boot process
        liftRpc $ do
          exportVmSwitcherInfo uuid
          stubdom <- getVmStubdom uuid
          when stubdom $ updateStubDomainID uuid
          stubdom_memory <- getVmStubdomMemory uuid
          stubdom_cmdline <- getVmStubdomCmdline uuid
          applyVmFirewallRules uuid
          whenDomainID_ uuid $ \domid -> do
            liftIO $ xsWrite (domainXSPath domid ++ "/v4v-firewall-ready") "1"

        waitForVmInternalState uuid Created 30
        -- BEFORE DEVICE MODEL
        info $ "pre-dm setup for " ++ show uuid
        liftRpc $ do 
          twiddlePermissions uuid
          setupCDDrives uuid
          --No longer passing v4v in the config, keep in db.
          v4v_enabled <- getVmV4V uuid
          when v4v_enabled $ setupV4VDevice uuid

          --setupBiosStrings uuid
          setupAcpiNode uuid
          -- some little network plumbing
          gives_network <- getVmProvidesNetworkBackend uuid
          when gives_network $ whenDomainID_ uuid $ \domid -> do
            liftIO $ xsWrite backendNode (show domid)
            liftIO $ xsChmod backendNode "r0"

          vfb_enabled <- getVmVfb uuid
          when vfb_enabled $ surfmanDbusCalls uuid

          vkb_enabled <- getVmVkbd uuid
          when vkb_enabled $ inputDbusCalls uuid
          info $ "done pre-dm setup for " ++ show uuid
         
        waitForVmInternalState uuid Created 60
        sentinel <- sentinelPath
        -- allow writing to sentinel
        maybe (return()) (\p -> liftIO $ xsWrite p "" >> xsChmod p "b0") sentinel
        
        -- AFTER DOMAIN CREATION
        liftRpc $ do
          -- assign sticky cd drives
          mapM_ (\d -> assignCdDevice d uuid) =<< getVmStickyCdDevices uuid
          info $ "unpause " ++ show uuid
          liftIO $ Xl.unpause uuid
        -- wait for bootup services to complete if using sentinel
        maybe (return()) (\p -> liftIO 
                            . void 
                            . timeout (10^6 * 60) 
                            . xsWaitFor p 
                            $ ((\v -> isJust v && v /= Just "") `fmap` xsRead p) 
                     ) sentinel
        
        applyVmBackendShift uuid
        return ()
      
      sentinelPath = do
        domid <- getDomainID uuid
        case domid of
          Nothing    -> return Nothing
          Just domid -> do
            s <- getVmBootSentinel uuid
            case s of
              Nothing -> return Nothing
              Just p  -> return (Just $ domainXSPath domid ++ "/" ++ p)

--FIXME: get rid of this when/if we remove dbusbouncer from ndvm
twiddlePermissions :: Uuid -> Rpc ()
twiddlePermissions uuid =
    -- make the /local/domain/<n>/vm node readable by all
    -- make the snapshot node readable/writable by itself
    whenDomainID_ uuid $ \domid -> let domid' = fromIntegral domid in
        liftIO $ do xsSetPermissions ("/local/domain/" ++ show domid ++ "/vm") [Permission 0 [PermRead]]
                    xsSetPermissions ("/vm/" ++ show uuid ++ "/uuid") [Permission 0 [PermRead]]
                    have_path <- xsRead (vmSuspendImageStatePath uuid)
                    when (have_path /= Nothing) $
                         xsSetPermissions (vmSuspendImageStatePath uuid) [Permission 0 [], Permission domid' [PermRead,PermWrite]]

removeVmEnvIso :: Uuid -> IO ()
removeVmEnvIso uuid = whenM (doesFileExist p) (removeFile p) where p = envIsoPath uuid

-- update the backend/frontend driver paths to point to new domains,
-- needed after backend reboot
applyVmBackendShift :: Uuid -> XM ()
applyVmBackendShift bkuuid = do
    target_ <- getDomainID bkuuid
    case target_ of
      Nothing -> warn $ printf "failed to move devices backend; domain %s does not exist" (show bkuuid)
      Just target ->
        do vms      <- filter ((/=) bkuuid) <$> (filterM isRunning =<< getVms)
           devices  <- liftRpc $ filter (uses bkuuid) . concat <$> mapM getdevs vms
           when (not . null $ devices) $ do
             info $ printf "moving device backends for %s" (show bkuuid)
             mapM_ (liftRpc . move target) devices

    where
      getdevs  uuid = zip (repeat uuid) <$> getdevs' uuid
      getdevs' uuid = whenDomainID [] uuid $ \domid -> do
        -- TODO: only supporting vif,vwif devices atm
        vifs  <- liftIO $ getFrontDevices VIF  domid
        vwifs <- liftIO $ getFrontDevices VWIF domid
        return (vifs ++ vwifs)
      uses bkuuid (_,d) = bkuuid == dmfBackUuid d
      move target (_,d) = moveBackend (dmfType d) (dmfDomid d) (dmfID d) target

disconnectFrontVifs :: Uuid -> Rpc ()
disconnectFrontVifs back_uuid =
    do vms      <- filter ((/=) back_uuid) <$> (filterM isRunning =<< getVms)
       devices  <- filter (uses back_uuid) . concat <$> mapM getdevs vms
       mapM_ disconnect devices

    where
      getdevs  uuid = zip (repeat uuid) <$> getdevs' uuid
      getdevs' uuid = whenDomainID [] uuid $ \domid -> do
        -- TODO: only supporting vif,vwif devices atm
        vifs  <- liftIO $ getFrontDevices VIF  domid
        vwifs <- liftIO $ getFrontDevices VWIF domid
        return (vifs ++ vwifs)
      uses bkuuid (_,d) = bkuuid == dmfBackUuid d
      disconnect (front_uuid, dev) = do
          let nid@(XbDeviceID nic_id) = dmfID dev
          info $ "disconnecting nic uuid=" ++ show front_uuid ++ " id=" ++ show nic_id
          liftIO $ Xl.connectVif front_uuid nid False


-- Reboot a VM
rebootVm :: Uuid -> Rpc ()
rebootVm uuid = do
    info $ "rebooting VM " ++ show uuid
    -- Write XL configuration file
    writeXlConfig =<< getVmConfig uuid True
    --Let xl take care of bringing down the domain and updating our state
    --When xenmgr sees the 'Rebooted' state, it fires off a startVm call,
    --which performs all the normal guest boot tasks, while xl brings up the domain.
    liftIO $ Xl.reboot uuid

shutdownVm :: Uuid -> Rpc ()
shutdownVm uuid = do
    info $ "shutting down VM " ++ show uuid
    acpi <- getVmAcpiState uuid
    use_agent <- RpcAgent.guestAgentRunning uuid
    -- if it is asleep, we need to wake it first
    when (acpi == 3) $ do
      info $ "resuming " ++ show uuid ++ " from S3 first.."
      resumeS3AndWaitS0 uuid
      info $ "resuming " ++ show uuid ++ " from S3 DONE."
    if use_agent
       then RpcAgent.shutdown uuid
       else liftIO $ Xl.shutdown uuid

forceShutdownVm :: Uuid -> Rpc ()
forceShutdownVm uuid = do
    info $ "forcibly shutting down VM " ++ show uuid
    liftIO $ Xl.destroy uuid

invokeShutdownVm :: Vm ()
invokeShutdownVm = liftRpc . shutdownVm =<< vmUuid

invokeForceShutdownVm :: Vm ()
invokeForceShutdownVm = liftRpc . forceShutdownVm =<< vmUuid

pauseVm :: Uuid -> Rpc ()
pauseVm uuid = do
  info $ "pausing VM " ++ show uuid
  liftIO $ Xl.pause uuid

unpauseVm :: Uuid -> Rpc ()
unpauseVm uuid = do
  info $ "unpausing VM " ++ show uuid
  liftIO $ Xl.unpause uuid

assertPvAddons :: Uuid -> Rpc ()
assertPvAddons uuid = getVmPvAddons uuid >>= \addons -> when (not addons) failActionRequiresPvAddons

sleepVm :: Uuid -> Rpc ()
sleepVm uuid = do
    acpi <- getVmAcpiState uuid
    case acpi of
      3 -> info $ show uuid ++ " is already in S3, not sending it into S3 again."
      _ -> do info $ "sending VM " ++ show uuid ++ " into S3"
              use_agent <- RpcAgent.guestAgentRunning uuid
              if use_agent
                 then RpcAgent.sleep uuid
                 else do assertPvAddons uuid
                         liftIO $ Xl.sleep uuid

hibernateVm :: Uuid -> Rpc ()
hibernateVm uuid = do
    info $ "sending VM " ++ show uuid ++ " into S4"
    running <- isRunning uuid
    when running $ do
      use_agent <- RpcAgent.guestAgentRunning uuid
      when (not use_agent) $ assertPvAddons uuid
      acpi <- getVmAcpiState uuid
      -- if it is asleep, we need to wake it first
      when (acpi == 3) $ resumeS3AndWaitS0 uuid
      if use_agent
         then RpcAgent.hibernate uuid
         else liftIO $ Xl.hibernate uuid
      saveConfigProperty uuid vmHibernated True

resumeS3AndWaitS0 :: Uuid -> Rpc ()
resumeS3AndWaitS0 uuid = do
  acpi <- getVmAcpiState uuid
  when (acpi == 3) $ do
    liftIO $ Xl.resumeFromSleep uuid
    done <- liftIO $ Xl.waitForAcpiState uuid 0 (Just 30)
    when (not done) $ warn $ "timeout waiting for S0 for " ++ show uuid

-- Execute action in parallel for each of given VM, returns results of actions. If any action
-- fails, we report an error through exception
parallelVmExec :: [Uuid] -> (Uuid -> Rpc a) -> Rpc [a]
parallelVmExec uuids f = do
    mvars   <- mapM handle uuids
    results <- mvars `seq` liftIO $ mapM takeMVar mvars
    mapM unwrap results
  where
    unwrap (Right v)  = return v
    unwrap (Left err) = throwError err

    handle uuid = do
      context <- rpcGetContext
      liftIO $ do
        r <- newEmptyMVar
        forkIO $ do
          -- execute in rpc monad
          rpcStatus <- rpc context (f uuid)
          -- give result or error
          putMVar r rpcStatus
        return r

-- Execute action in stages, for each vm type, each stage is done in parallel but the stages itself
-- are sequential
parallelVmExecByType :: [VmType] -> (Uuid -> Rpc a) -> Rpc [(Uuid, a)]
parallelVmExecByType types f =
    concat <$> mapM run types
  where
    run t = getVmsByType t         >>= \uuids   ->
            parallelVmExec uuids f >>= \results ->
            return $ zip uuids results

-- paralell execution in explicitly specified stages (as sets of uuids). Stages are sequential, actions
-- in a stage are parallel
parallelVmExecInStages :: [[Uuid]] -> (Uuid -> Rpc a) -> Rpc [(Uuid, a)]
parallelVmExecInStages stages f =
    concat <$> mapM run stages
    where run uuids = parallelVmExec uuids f >>= \results ->
                      return $ zip uuids results

-- Switch to a VM
switchVm :: MonadRpc e m => Uuid -> m Bool
switchVm uuid = whenDomainID False uuid $ \domid -> do
    debug $ "Attempting to switch screen to domain " ++ show uuid
    -- ensure switcher is ready
    liftIO $ Xl.wakeIfS3 uuid
    success <- inputSwitchFocus domid
    when (not success) $ warn ("switchVm: failed for uuid " ++ show uuid)
    return success

-- Switch to a VM, and repeat the attempts until it succeeds
reallySwitchVm :: MonadRpc e m => Uuid -> Float -> m Bool
reallySwitchVm uuid timeout_time =
    do t0 <- liftIO getCurrentTime
       loop t0
    where
      loop t0 =
          do t1 <- liftIO getCurrentTime
             let d = realToFrac $ diffUTCTime t1 t0
             if d >= timeout_time || d < 0
                then return False
                else do
                  r <- switchVm uuid
                  if r then return True else liftIO (threadDelay (2*10^5)) >> loop t0

-- Switch to graphics fallback vm
switchGraphicsFallback :: Rpc Bool
switchGraphicsFallback = do
  vm <- getGraphicsFallbackVm
  case vm of
    Nothing  -> return False
    Just vm  -> switchVm vm

-- This just sets authentication context on input daemon
loginToVm :: Uuid -> Rpc ()
loginToVm uuid = do
  -- typically this is actually a HASH not a real user id
  user <- readConfigProperty uuid vmCryptoUser
  case user of
    Nothing  -> return ()
    Just (uid :: String) -> inputAuthSetContextFlags uid "" (auth_FLAG_REMOTE_USER .|.
                                                             auth_FLAG_USER_HASH)

allocateDiskID :: Uuid -> Rpc DiskID
allocateDiskID uuid =
    do disks <- getDisks uuid
       return $ (maxID (M.keys disks)) + 1
  where
    maxID [] = (-1)
    maxID xs = maximum xs

allocateNicID :: Uuid -> Rpc NicID
allocateNicID uuid =
    getNicIds uuid >>= \ids -> return (XbDeviceID $ (maxID ids) + 1)
  where
    maxID [] = (-1)
    maxID xs = maximum . map xbdevID $ xs

modifyDBVmFirewallRules :: Uuid -> ([Firewall.Rule] -> [Firewall.Rule]) -> Rpc ()
modifyDBVmFirewallRules uuid f = do
    r <- getVmFirewallRules uuid
    let r' = nub $ f r
    when ( r /= r' ) $ saveConfigProperty uuid vmFirewallRules r'

addVmFirewallRule :: Uuid -> Firewall.Rule -> Rpc ()
addVmFirewallRule uuid rule = modifyDBVmFirewallRules uuid (++ [rule])

deleteVmFirewallRule :: Uuid -> Firewall.Rule -> Rpc ()
deleteVmFirewallRule uuid rule = modifyDBVmFirewallRules uuid (filter (/= rule))

getEffectiveVmFirewallRules :: Uuid -> Rpc [Firewall.Rule]
getEffectiveVmFirewallRules uuid =
    getVmFirewallRules uuid >>= \rules -> return $ nub (rules ++ map Firewall.inverse rules)


doVmFirewallRules :: Rpc () -> Rpc ([Firewall.ActiveVm], [Firewall.ActiveVm]) -> Rpc ()
doVmFirewallRules message which =
    whenM appGetV4VFirewall $ do
        message
        (vms, vms') <- which
        seamlessVms <- getSeamlessVms

        info $ "firewall rule delta vms:" 
        info $ "BEFORE: " ++ show vms
        info $ "NOW: " ++ show vms
        
        let reduce vms ((Firewall.ActiveVm _ _ vm _ _), rule) =
                Firewall.reduce (Firewall.ReduceContext vm seamlessVms vms) [rule]
            vm_rules = mapM (getEffectiveVmFirewallRules . Firewall.vmUuid)
        let makeRules vms = (concat . map (reduce vms) .
                	     concat . zipWith (\vm rs -> map (vm,) rs) vms) <$>
 		             vm_rules vms
        changeset <- Firewall.changeset <$> makeRules vms <*> makeRules vms'
        liftIO $ Firewall.applyChangeset changeset

applyVmFirewallRules :: Uuid -> Rpc ()
applyVmFirewallRules uuid = doVmFirewallRules (info $ "applying v4v firewall rules due to " ++ show uuid) $
                            do active <- activeVms
                               return (filter (\vm -> Firewall.vmUuid vm /= uuid) active,
                                       active)

unapplyVmFirewallRules :: Uuid -> Rpc ()
unapplyVmFirewallRules uuid = doVmFirewallRules (info $ "unapplying v4v firewall rules due to " ++ show uuid) $
                              do active <- activeVms
                                 myself <- getActiveVm uuid
                                 return (nub $ active ++ maybeToList myself
                                        , filter (\vm -> Firewall.vmUuid vm /= uuid) active)

getActiveVm :: Uuid -> Rpc (Maybe Firewall.ActiveVm)
getActiveVm uuid = do
  maybe_domid <- liftIO . xsRead $ "/xenmgr/vms/" ++ show uuid ++ "/domid"
  stubdom <- getStubDomainID uuid
  typ <- typStr <$> getVmType uuid
  name <- getVmName uuid
  return . fmap (\domid -> Firewall.ActiveVm domid stubdom uuid typ name) . fmap read $ maybe_domid
  where typStr (ServiceVm tag) = tag
        typStr Svm = "svm"
    
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM op = liftM catMaybes . mapM op

-- this resolves domid using xenstore as the domain might well be destroyed on xen level when this info is needed
activeVms :: Rpc [Firewall.ActiveVm]
activeVms =
    mapMaybeM getActiveVm =<< filterM isRunning =<< getVms

addNicToVm :: Uuid -> NicDef -> Vm NicID
addNicToVm uuid nic = withVmDbLock . liftRpc $
    do info $ "adding virtual nic to VM " ++ show uuid
       nics <- getVmNicDefs uuid
       id   <- allocateNicID uuid
       let nics' = M.insert id (nic { nicdefId = id }) nics
       saveConfigProperty uuid vmNics nics'
       return id

addDefaultNicToVm :: Uuid -> Vm NicID
addDefaultNicToVm uuid = addNicToVm uuid nic
    where
      nic = NicDef { nicdefId = XbDeviceID (-1)
                   , nicdefNetwork = fallbackNetwork
                   , nicdefWirelessDriver = False
                   , nicdefBackendUuid = Nothing
                   , nicdefBackendName = Nothing
                   , nicdefBackendDomid = Nothing
                   , nicdefEnable = True
                   , nicdefMac = Nothing }

addDiskToVm :: Uuid -> Disk -> Vm DiskID
addDiskToVm uuid disk = withVmDbLock . liftRpc $
    do info $ "adding virtual disk to VM " ++ show uuid
       disks <- getDisks uuid
       id    <- allocateDiskID uuid
       let disks' = M.insert id disk disks
       saveConfigProperty uuid vmDisks disks'
       return id

--
removeNicFromVm :: Uuid -> NicID -> Vm ()
removeNicFromVm uuid id = withVmDbLock . liftRpc $
    do info $ "removing virtual nic " ++ show id ++ " from VM " ++ show uuid
       nics <- getVmNicDefs uuid
       saveConfigProperty uuid vmNics (M.delete id nics)

addDefaultDiskToVm :: Uuid -> Vm DiskID
addDefaultDiskToVm uuid =
    do virt_path <- pickDiskVirtPath uuid
       addDiskToVm uuid $
                Disk { diskPath = ""
                     , diskType = VirtualHardDisk
                     , diskMode = Vm.Types.ReadWrite
                     , diskDeviceType = DiskDeviceTypeDisk
                     , diskDevice = virt_path
                     , diskSnapshotMode = Nothing
                     , diskSha1Sum = Nothing
                     , diskManagedType = UnmanagedDisk
                     , diskShared = False
                     , diskEnabled = True
                     }

--
addVhdDiskToVm :: Uuid -> FilePath -> Vm DiskID
addVhdDiskToVm uuid path = do
    virt_path <- pickDiskVirtPath uuid
    let disk = Disk {
                 diskPath = path
               , diskType = VirtualHardDisk
               , diskMode = Vm.Types.ReadWrite
               , diskDeviceType = DiskDeviceTypeDisk
               , diskDevice = virt_path
               , diskSnapshotMode = Nothing
               , diskSha1Sum = Nothing
               , diskManagedType = UnmanagedDisk
               , diskShared = False
               , diskEnabled = True
               }
    addDiskToVm uuid disk

--
addPhyDiskToVm :: Uuid -> FilePath -> Vm DiskID
addPhyDiskToVm uuid path = do
    virt_path <- pickDiskVirtPath uuid
    let disk = Disk {
                 diskPath = path
               , diskType = PhysicalDevice
               , diskMode = Vm.Types.ReadWrite
               , diskDeviceType = DiskDeviceTypeDisk
               , diskDevice = virt_path
               , diskSnapshotMode = Nothing
               , diskSha1Sum = Nothing
               , diskManagedType = UnmanagedDisk
               , diskShared = False
               , diskEnabled = True
               }
    addDiskToVm uuid disk

--
removeDiskFromVm :: Uuid -> DiskID -> Vm ()
removeDiskFromVm uuid id = withVmDbLock . liftRpc $
    do info $ "removing a virtual disk " ++ show id ++ " from VM " ++ show uuid
       disks <- getDisks uuid
       case M.lookup id disks of
         Nothing -> return ()
         Just d  -> do
           let disks' = M.delete id disks
           removeDiskFiles uuid d
           saveConfigProperty uuid vmDisks disks'
       
removeDiskFiles :: Uuid -> Disk -> Rpc ()
removeDiskFiles uuid d = removeVhd d where
    removeVhd d
      | not (diskShared d), diskType d == VirtualHardDisk
      = do refs <- nub . map fst <$> getVhdReferences (diskPath d)
           -- only remove when only this vm references the vhd
           when (refs == [uuid]) $ do
             let p = diskPath d
             liftIO . whenM (doesFileExist p) $ do
               info $ "Removing VHD file " ++ p
               removeLink p
    removeVhd _ = return ()

--
createAndAddDiskToVm :: Uuid -> Int -> Vm DiskID
createAndAddDiskToVm uuid sizeGB =
    do let sizeMB = sizeGB * 1024
       vhdPath <- liftIO $ createVhd sizeMB
       addVhdDiskToVm uuid vhdPath

modifyVmNic :: Uuid -> NicID -> (NicDef -> NicDef) -> Vm ()
modifyVmNic uuid nicID modifyF = withVmDbLock . liftRpc $
    do n <- getNic uuid nicID
       case n of
         Nothing  -> failNoSuchNic
         Just nic -> let nic' = modifyF nic in
                     saveConfigProperty uuid (vmNic nicID) nic'

modifyVmDisk :: Uuid -> DiskID -> (Disk -> Disk) -> Vm ()
modifyVmDisk uuid diskID modifyF = withVmDbLock . liftRpc $
    do p <- readConfigProperty uuid (vmDisk diskID)
       case p of
         Nothing   -> failNoSuchDisk
         Just disk ->
             let disk' = modifyF disk in
             saveConfigProperty uuid (vmDisk diskID) disk'

modifyVmPciPtRules :: Uuid -> (PciPtRuleMap -> PciPtRuleMap) -> Rpc ()
modifyVmPciPtRules uuid modifyF =
    do rules <- getPciPtRules uuid
       let rules' = modifyF rules
       saveConfigProperty uuid vmPcis rules'

finally' = flip E.finally

mountVmDisk :: Uuid -> DiskID -> Bool -> FilePath -> Rpc ()
mountVmDisk uuid diskID readonly path =
    do disk <- getDisk uuid diskID
       case disk of
         Nothing   -> failNoSuchDisk
         Just disk -> case diskType disk of
                        VirtualHardDisk -> mountVhd $ diskPath disk
                        _               -> failIncorrectDiskType
    where
      mountVhd :: FilePath -> Rpc ()
      mountVhd vhdpath =
          do keydirs <- concat . intersperse "," <$> getCryptoKeyLookupPaths uuid
             liftIO $ do
               dev <- tapCreate "vhd" [("TAPDISK2_CRYPTO_KEYDIR", keydirs)] readonly vhdpath
               E.handle (\(e :: E.SomeException) -> removedev dev >> E.throw e) $ do
                 xsWrite (xspath ++ "/dev") dev
                 let mountopts =
                       if readonly then ["-o", "ro"] else []
                 readProcessOrDie "mount" (mountopts ++ [dev, path]) ""
                 xsWrite (xspath ++ "/path") path
                 return ()
          where
            removedev path = do
              tapDestroy path
              xsRm xspath
            xspath = "/xenmgr/mount/" ++ show uuid ++ "/" ++ show diskID

unmountVmDisk :: Uuid -> DiskID -> Rpc ()
unmountVmDisk uuid diskID =
    do disk <- getDisk uuid diskID
       case disk of
         Nothing   -> failNoSuchDisk
         Just disk -> case diskType disk of
                        VirtualHardDisk -> unmountVhd $ diskPath disk
                        _               -> failIncorrectDiskType
    where
      unmountVhd :: FilePath -> Rpc ()
      unmountVhd vhdpath = liftIO $
          do dev <- fromMaybe "" <$> xsRead (xspath ++ "/dev")
             mountpath <- xsRead (xspath ++ "/path")
             case mountpath of
               Nothing -> error $ "device not mounted " ++ show dev
               Just mountpath -> do
                 readProcessOrDie "umount" [mountpath] ""
                 tapDestroy vhdpath
                 xsRm xspath
                 return ()
          where
            xspath = "/xenmgr/mount/" ++ show uuid ++ "/" ++ show diskID

generateCryptoKeyIn :: Uuid -> DiskID -> Int -> FilePath -> Rpc ()
generateCryptoKeyIn vm diskID keybits dir
  = do when (not $ keybits `elem` [256, 512]) $ error "only supported key sizes: 256, 512"
       disk <- haveDisk =<< getDisk vm diskID
       checkFileEx disk
       checkKeySet disk
       liftIO (setKeyHash disk =<< mkRandomKeyFile disk)
    where
      checkFileEx d = whenM (liftIO $ doesFileExist $ keyfile d) $ failCryptoKeyAlreadyExists
      checkKeySet d = whenM ((/= "none") <$> liftIO (readKeyHash d)) $ failVhdKeyAlreadySet
      readKeyHash d = strip <$> readProcessOrDie "vhd-util" ["key", "-n", diskPath d, "-p"] []
      setKeyHash d f = void $ readProcessOrDie "vhd-util" ["key", "-n", diskPath d, "-k", f, "-s"] []
      mkRandomKeyFile d =
        let dst = keyfile d
            src = "/dev/random" in
        copy src dst (fromIntegral keysize_bytes) >> return dst
      copy s d n = BL.readFile s >>= return . BL.take n >>= BL.writeFile d
      keyfile d = dir </> diskUuid d ++ ",aes-xts-plain," ++ show keybits ++ ".key"
      -- 512 keysize by default (this means AES-256 as aes-xts uses keys split in half)
      keysize_bytes = keybits `div` 8
      haveDisk Nothing  = failNoSuchDisk
      haveDisk (Just d) = return d
      diskUuid = takeBaseName . diskPath

generateCryptoKey :: Uuid -> DiskID -> Int -> Rpc ()
generateCryptoKey vm diskID keybits
  = into =<< ( split ',' <$> appGetPlatformCryptoKeyDirs )
    where
      into [] = error "no platform crypto directories configured"
      into (p:_) =
        do liftIO $ whenM (not <$> doesDirectoryExist p) $ createDirectory p
           generateCryptoKeyIn vm diskID keybits p

-- only computes hash for disk of id 0. TODO: make this more generic maybe
addVmDiskHashes :: Vm ()
addVmDiskHashes = vmUuid >>= \uuid -> addTo uuid 0
    where
      addTo uuid id =
          do Just primary <- M.lookup id <$> (liftRpc $ getDisks uuid)
             case diskSha1Sum primary of
               Just _ -> return () -- already have
               Nothing ->
                   liftRpc (computeDiskSha1Sum uuid primary) >>= \sum ->
                       modifyVmDisk uuid id $ \disk -> disk { diskSha1Sum = Just sum }

checkVmDiskHashes :: Vm Bool
checkVmDiskHashes =
    do bypass <- liftRpc appBypassSha1SumChecks
       if not bypass
          then do
            uuid  <- vmUuid
            disks <- M.elems <$> liftRpc (getDisks uuid)
            all (==True) <$> mapM (validate uuid) disks
          else return True
    where
    validate uuid d
      | Just sh <- sha1 = liftRpc ( computeDiskSha1Sum uuid d ) >>= \sh' -> equalT sh sh'
      | otherwise       = return True
               where
                 sha1 = diskSha1Sum d
                 path = diskPath d
                 equalT sh sh' | sh == sh' = return True
                               | otherwise = vmSubmit (VmMeasurementFailure path sh sh') >> return False

getMeasureFailAction :: Rpc PMAction
getMeasureFailAction = dbReadWithDefault ActionForcedShutdown "/xenmgr/measure-fail-action"

setMeasureFailAction :: PMAction -> Rpc ()
setMeasureFailAction a = dbWrite "/xenmgr/measure-fail-action" a

tapEnvForVm :: Uuid -> Rpc [(String,String)]
tapEnvForVm uuid = do
  keydirs <- concat . intersperse "," <$> getCryptoKeyLookupPaths uuid
  return [("TAPDISK2_CRYPTO_KEYDIR", keydirs)]

tapCreateForVm :: Uuid -> Bool -> FilePath -> Rpc FilePath
tapCreateForVm uuid ro path = do
  env <- tapEnvForVm uuid
  liftIO $ tapCreateVhd env ro path
  
-- Computer sha1 sum for disk. Has to be through tap device because the vhd file changes
-- even for completely readonly fs
computeDiskSha1Sum :: Uuid -> Disk -> Rpc Integer
computeDiskSha1Sum vm_uuid d
    | diskType d == VirtualHardDisk =
        do tapdev <- tapCreateForVm vm_uuid True (diskPath d)
           liftIO $
             E.finally (fileSha1Sum tapdev) (spawnShell' $ "tap-ctl destroy -d " ++ tapdev)
    | diskType d == Aio = liftIO $ 
        do tapdev <- fromMaybe (error $ "FAILED to create tap device for " ++ diskPath d ++ ", possibly in use?")
                     . fmap chomp
                    <$> (spawnShell' $ "tap-ctl create -a aio:" ++ (diskPath d))
           E.finally (fileSha1Sum tapdev) (spawnShell' $ "tap-ctl destroy -d " ++ tapdev)
    | diskType d `elem` [PhysicalDevice, DiskImage] = liftIO $
          fileSha1Sum (diskPath d)
    | otherwise = error "unsupported disk type, should be vhd or phy or file"

parseKernelExtract :: String -> (Maybe DiskID, Maybe PartitionNum, FilePath)
parseKernelExtract p
  = case split ':' p of
      [file] -> (Nothing, Nothing, file)
      [opts, file] ->
        case split ',' p of
          [diskS,partS] ->
            let diskNum = fromMaybe (error "bad disk number in kernel-extract") $ maybeRead diskS in
            let partNum = fromMaybe (error "bad partition number in kernel-extract") $ maybeRead partS in
            (Just diskNum, Just partNum, file)
          [diskS] ->
            let diskNum = fromMaybe (error "bad disk number in kernel-extract") $ maybeRead diskS in
            (Just diskNum, Nothing, file)
          _ -> error "incorrect disk & partition specification in kernel-extract"
      _ -> error "incorrect kernel-extract syntax"

-- Assumes that kernel is stored in disk of id 0 for that vm
extractKernelFromPvDomain :: Uuid -> Rpc ()
extractKernelFromPvDomain uuid = join $ extractFileFromPvDomain <$> getVmKernelPath uuid <*> getVmKernelExtract uuid <*> pure uuid
extractInitrdFromPvDomain :: Uuid -> Rpc ()  
extractInitrdFromPvDomain uuid = do
  initrd <- getVmInitrd uuid
  let initrd' = case initrd of
        "" -> Nothing
        _  -> Just initrd
  join $ extractFileFromPvDomain <$> pure initrd' <*> getVmInitrdExtract uuid <*> pure uuid

extractFileFromPvDomain :: Maybe FilePath -> String -> Uuid -> Rpc ()
extractFileFromPvDomain Nothing _ _ = return ()
extractFileFromPvDomain (Just dst_path) ext_loc uuid = withKernelPath dst_path where
  withKernelPath dst_kernel_path = do
       let (diskid,partid,src_path) = parseKernelExtract ext_loc
       disk <- getDisk uuid (fromMaybe 0 diskid)
       keydirs <- concat . intersperse "," <$> getCryptoKeyLookupPaths uuid
       case (disk, dst_kernel_path) of
         (_, "") -> return () -- doesn't have a kernel, not a pv domain, ignore
         (Nothing, _) -> error "extract-kernel: domain does not have a disk with ID 0"
         (_, path) | null src_path -> return () -- no extraction
         (Just disk, path) -> do liftIO $ copyKernelFromDisk [("TAPDISK2_CRYPTO_KEYDIR", keydirs)] disk path (partid,src_path)
                                 info $ "extracted pv kernel/initrd from " ++ src_path ++ " into " ++ path

copyKernelFromDisk :: [ (String, String) ] -> Disk -> FilePath -> (Maybe PartitionNum,FilePath) -> IO ()
copyKernelFromDisk extraEnv disk dst_path src
 = copyFileFromDisk extraEnv (diskType disk) (diskMode disk == ReadOnly) (diskPath disk) src dst_path

changeVmNicNetwork :: Uuid -> NicID -> Network -> XM ()
changeVmNicNetwork uuid nicid network = do
  -- Save in database
  xmRunVm uuid $ modifyVmNic uuid nicid $ \nic -> nic { nicdefNetwork = network }
  -- Hotswap network if VM is running
  whenM (isRunning uuid) $ do
    xmWithNetsyncLock . liftRpc $
       do -- disconnect vif
          -- notify xenvm TODO: maybe won't be necessary sometime?
          -- notify network daemon
          -- resynchronise vif state
          info $ "====In ChangeVmNicNetwork====="
          liftIO $ Xl.connectVif uuid nicid False
          liftIO $ Xl.changeNicNetwork uuid nicid network
          whenDomainID_ uuid $ \domid -> joinNetwork network domid nicid
-- Property accessors
---------------------
setVmWiredNetwork :: Uuid -> Network -> XM ()
setVmWiredNetwork uuid network
    = getVmWiredNics uuid >>= pure . take 1 >>= mapM_ (\n -> changeVmNicNetwork uuid (nicdefId n) network)

setVmWirelessNetwork :: Uuid -> Network -> XM ()
setVmWirelessNetwork uuid network
    = getVmWirelessNics uuid >>= pure . take 1 >>= mapM_ (\n -> changeVmNicNetwork uuid (nicdefId n) network)

-- TODO: this sucks
-- update 6.05.2011: sucks a little bit less now but still
setVmGpu :: Uuid -> String -> Rpc ()
setVmGpu uuid s = do
    running <- isRunning uuid
    when running $ failCannotTurnHdxWhenVmRunning
    case s of
      "hdx" -> test_hdx
      _ -> return ()
    saveConfigProperty uuid vmGpu s
    where
      set_hdx uuid cfgs = map set cfgs
          where set (uuid',c) | uuid == uuid' = (uuid', c { vmcfgGraphics = HDX })
                              | otherwise     = (uuid', c)
      test_hdx = do
        unlessM (getVmPvAddons uuid) $ failCannotTurnHdxWithoutPvAddons
        verifyAutostartAndHDX (set_hdx uuid)

verifyAutostartAndHDX :: ([(Uuid,VmConfig)] -> [(Uuid,VmConfig)]) -> Rpc ()
verifyAutostartAndHDX change = do
  max_vgpus <- getMaxVgpus
  vms <- getGuestVms
  cfgs <- zip vms <$> mapM (\uuid -> getVmConfig uuid False) vms
  let cfgs' = change cfgs
      offending (uuid,cfg) = vmcfgGraphics cfg == HDX && vmcfgAutostart cfg
  when (length (filter offending cfgs') > max_vgpus) $ failSimultaneousAutostartAndHdx

setVmCd :: Uuid -> String -> Rpc ()
setVmCd uuid str =
    -- change all cdrom paths
    readConfigPropertyDef uuid vmDisks [] >>=
    mapM maybeChange                      >>=
    saveConfigProperty uuid vmDisks
  where
    maybeChange disk | not (isCdrom disk) = return disk
                     | otherwise          = do isos <- appIsoPath
                                               let path = isos ++ "/" ++ name
                                               -- hot swap cd
                                               whenVmRunning uuid (liftIO $ Xl.changeCd uuid path)
                                               return $ disk { diskPath = path }
    name | str == ""   = "null.iso"
         | otherwise   = str

setVmType :: Uuid -> VmType -> Rpc ()
setVmType uuid typ = saveConfigProperty uuid vmType typ

setVmAmtPt :: Uuid -> Bool -> Rpc ()
setVmAmtPt uuid amtpt = saveConfigProperty uuid vmAmtPt amtpt

setVmSeamlessTraffic :: Uuid -> Bool -> Rpc ()
setVmSeamlessTraffic uuid view = saveConfigProperty uuid vmSeamlessTraffic view

setVmStartOnBoot :: Uuid -> Bool -> Rpc ()
setVmStartOnBoot uuid start = do
    when start $ verifyAutostartAndHDX (set_start uuid)
    saveConfigProperty uuid vmStartOnBoot start
  where
    set_start uuid cfgs = map set cfgs
        where set (uuid',c) | uuid == uuid' = (uuid', c { vmcfgAutostart = True })
                            | otherwise     = (uuid', c)

setVmHiddenInSwitcher :: Uuid -> Bool -> Rpc ()
setVmHiddenInSwitcher uuid hidden = do
    saveConfigProperty uuid vmHidden hidden
    exportVmSwitcherInfo uuid

setVmHiddenInUi :: Uuid -> Bool -> Rpc ()
setVmHiddenInUi uuid hidden = saveConfigProperty uuid vmHiddenInUi hidden

-- in mebibytes
setVmMemory :: Uuid -> Int -> Rpc ()
setVmMemory uuid mb = do
    saveConfigProperty uuid vmMemory mb
    -- running <- isRunning uuid
    -- when running $ Xenvm.setMemTarget uuid mb

-- <= 0 == turned off
setVmMemoryStaticMax :: Uuid -> Int -> Rpc ()
setVmMemoryStaticMax uuid mib = do
    saveOrRmConfigProperty uuid vmMemoryStaticMax (if mib <= 0 then Nothing else Just mib)

-- <= 0 == turned off
setVmMemoryMin :: Uuid -> Int -> Rpc ()
setVmMemoryMin uuid mib = do
  saveOrRmConfigProperty uuid vmMemoryMin (if mib <= 0 then Nothing else Just mib)

setVmName :: Uuid -> String -> Rpc ()
setVmName uuid name = do
  saveConfigProperty uuid vmName (strip name)
  exportVmSwitcherInfo uuid
  notifyVmNameChanged uuid

setVmImagePath :: Uuid -> FilePath -> Rpc ()
setVmImagePath uuid path = do
  saveConfigProperty uuid vmImagePath (strip path)
  exportVmSwitcherInfo uuid

-- swaps slots as necessary, requires locking
setVmSlot :: Uuid -> Int -> XM ()
setVmSlot uuid slot
  = xmWithVmSlotLock . liftRpc $ swap
  where
    swap = do
      there <- if slot < 0 then return Nothing else slotted slot
      case there of
        Nothing -> really_save uuid slot
        Just vm -> getVmSlot uuid >>= \prev ->
                   when (prev>0) (really_save vm prev) >> really_save uuid slot

    slotted at = fmap listToMaybe $ filterM (\uuid -> (at ==) <$> getVmSlot uuid) =<< getVms

    really_save uuid slot  = getDomainID uuid >>= set where
          set (Just domid) = inputSetSlot domid slot >> saveConfigProperty uuid vmSlot slot
          set _            = saveConfigProperty uuid vmSlot slot

setVmPvAddons uuid adds = saveConfigProperty uuid vmPvAddons ( adds :: Bool )
setVmPvAddonsVersion uuid v = saveConfigProperty uuid vmPvAddonsVersion ( v :: String )
setVmTimeOffset uuid o = saveConfigProperty uuid vmTimeOffset ( o :: Int )
setVmCryptoUser uuid u = saveOrRmConfigProperty uuid vmCryptoUser (if u == "" then Nothing else Just u)
setVmCryptoKeyDirs uuid d = saveConfigProperty uuid vmCryptoKeyDirs ( d :: String )
setVmAutoS3Wake uuid a = saveConfigProperty uuid vmAutoS3Wake ( a :: Bool )

setVmNotify uuid v = saveConfigProperty uuid vmNotify (v::String)
setVmHvm uuid v = saveConfigProperty uuid vmHvm (v::Bool)
setVmPae uuid v = saveConfigProperty uuid vmPae (v::Bool)
setVmApic uuid v = saveConfigProperty uuid vmApic (v::Bool)
setVmAcpi uuid v = saveConfigProperty uuid vmAcpi (v::Bool)
setVmViridian uuid v = saveConfigProperty uuid vmViridian (v::Bool)
setVmNx uuid v = saveConfigProperty uuid vmNx (v::Bool)
setVmSound uuid v = saveConfigProperty uuid vmSound (v::String)
setVmDisplay uuid v = saveConfigProperty uuid vmDisplay (v::String)
setVmBoot uuid v = saveConfigProperty uuid vmBoot (v::String)
setVmCmdLine uuid v = saveConfigProperty uuid vmCmdLine (v::String)
setVmKernel uuid v = saveConfigProperty uuid vmKernel (v::String)
setVmKernelExtract uuid v = saveConfigProperty uuid vmKernelExtract (v::String)
setVmInitrd uuid v = saveConfigProperty uuid vmInitrd (v::String)
setVmInitrdExtract uuid v = saveConfigProperty uuid vmInitrdExtract (v::String)
setVmAcpiPath uuid v = saveConfigProperty uuid vmAcpiPath (v::String)
setVmVcpus uuid v = saveConfigProperty uuid vmVcpus (v::Int)
setVmCoresPerSocket uuid v = saveConfigProperty uuid vmCoresPerSocket (v::Int)
setVmVideoram uuid v = saveConfigProperty uuid vmVideoram (v::Int)
setVmPassthroughMmio uuid v = saveConfigProperty uuid vmPassthroughMmio (v::String)
setVmPassthroughIo uuid v = saveConfigProperty uuid vmPassthroughIo (v::String)
setVmFlaskLabel uuid v = saveConfigProperty uuid vmFlaskLabel (v::String)
setVmHap uuid v = saveConfigProperty uuid vmHap (v::Bool)
setVmSmbios uuid v = saveConfigProperty uuid vmSmbios (v::String)
setVmDescription uuid v = saveConfigProperty uuid vmDescription (v::String)
setVmStartOnBootPriority uuid v = saveConfigProperty uuid vmStartOnBootPriority (v::Int)
setVmKeepAlive uuid v = saveConfigProperty uuid vmKeepAlive (v::Bool)
setVmProvidesNetworkBackend uuid v = saveConfigProperty uuid vmProvidesNetworkBackend (v::Bool)
setVmProvidesDefaultNetworkBackend uuid v = saveConfigProperty uuid vmProvidesDefaultNetworkBackend (v::Bool)
setVmProvidesGraphicsFallback uuid v = saveConfigProperty uuid vmProvidesGraphicsFallback (v::Bool)
setVmShutdownPriority uuid v = saveConfigProperty uuid vmShutdownPriority (v::Int)
setVmSeamlessId uuid v = saveConfigProperty uuid vmSeamlessId (v::String)
setVmQemuDmPath uuid v = saveConfigProperty uuid vmQemuDmPath (v::String)
setVmQemuDmTimeout uuid v = saveConfigProperty uuid vmQemuDmTimeout (v::Int)
setVmControlPlatformPowerState uuid v = saveConfigProperty uuid vmControlPlatformPowerState (v::Bool)

setVmExtraXenvm uuid str = saveConfigProperty uuid vmExtraXenvm (filter (not.null) . map strip . split ';' $ str)
setVmExtraHvm   uuid str = saveConfigProperty uuid vmExtraHvms (filter (not.null) . map strip . split ';' $ str)
setVmStartFromSuspendImage uuid v = saveConfigProperty uuid vmStartFromSuspendImage (v::String)

setVmTrackDependencies uuid v = saveConfigProperty uuid vmTrackDependencies (v::Bool)
setVmSeamlessMouseLeft uuid v =
    do saveConfigProperty uuid vmSeamlessMouseLeft (v::String)
       inputUpdateSeamlessMouseSettings uuid

setVmSeamlessMouseRight uuid v =
    do saveConfigProperty uuid vmSeamlessMouseRight (v::String)
       inputUpdateSeamlessMouseSettings uuid

setVmOs uuid os = saveConfigProperty uuid vmOs (osToStr os)
setVmOemAcpiFeatures uuid v = saveConfigProperty uuid vmOemAcpiFeatures (v::Bool)
setVmUsbEnabled uuid v = saveConfigProperty uuid vmUsbEnabled (v::Bool)
setVmUsbControl uuid v =
    do saveConfigProperty uuid vmUsbControl (v::Bool)
       -- TODO: surely this can be improved, for now SIGHUP to cause proxy daemon
       -- to reevaluate rules
       liftIO $ spawnShell' "killall -SIGHUP rpc-proxy"
       return ()
setVmUsbAutoPassthrough uuid v = saveConfigProperty uuid vmUsbAutoPassthrough (v::Bool)
setVmStubdom uuid v = saveConfigProperty uuid vmStubdom (v::Bool)
setVmStubdomMemory uuid v = saveConfigProperty uuid vmStubdomMemory (v::Int)
setVmStubdomCmdline uuid v = saveConfigProperty uuid vmStubdomCmdline (v::String)
setVmCpuid uuid v = saveConfigProperty uuid vmCpuid (v::String)
setVmXciCpuidSignature uuid v = saveConfigProperty uuid vmXciCpuidSignature (v::Bool)
setVmGreedyPcibackBind uuid v = saveConfigProperty uuid vmGreedyPcibackBind (v::Bool)

setVmRunPostCreate uuid v = saveOrRmConfigProperty uuid vmRunPostCreate (v::Maybe String)
setVmRunPreDelete uuid v = saveOrRmConfigProperty  uuid vmRunPreDelete (v::Maybe String)
setVmRunPreBoot uuid v = saveOrRmConfigProperty  uuid vmRunPreBoot (v::Maybe String)
setVmRunInsteadofStart uuid v = saveOrRmConfigProperty uuid vmRunInsteadofStart (v::Maybe String)
setVmRunOnStateChange uuid v = saveOrRmConfigProperty uuid vmRunOnStateChange (v::Maybe String)
setVmRunOnAcpiStateChange uuid v = saveOrRmConfigProperty uuid vmRunOnAcpiStateChange (v::Maybe String)

setVmNativeExperience uuid v
  = do mapM_ clearSetting =<< getGuestVms
       saveConfigProperty uuid vmNativeExperience (v :: Bool)
  where clearSetting uuid = saveConfigProperty uuid vmNativeExperience False

setVmShowSwitcher uuid v = saveConfigProperty uuid vmShowSwitcher (v :: Bool)
setVmWirelessControl uuid v = saveConfigProperty uuid vmWirelessControl (v :: Bool)
setVmUsbGrabDevices uuid v = saveConfigProperty uuid vmUsbGrabDevices (v::Bool)
setVmS3Mode uuid v = saveConfigProperty uuid vmS3Mode (v::S3Mode)
setVmS4Mode uuid v = saveConfigProperty uuid vmS4Mode (v::S4Mode)
setVmVsnd uuid v = saveConfigProperty uuid vmVsnd (v::Bool)
setVmRealm uuid v = saveConfigProperty uuid vmRealm (v::String)
setVmSyncUuid uuid v = saveConfigProperty uuid vmSyncUuid (v::String)
setVmIcbinnPath uuid v = saveConfigProperty uuid vmIcbinnPath (v::String)
setVmOvfTransportIso uuid = saveConfigProperty uuid vmOvfTransportIso
setVmDownloadProgress uuid v = do
  dbWrite ("/vm/"++show uuid++"/download-progress") (v::Int)
  notifyVmTransferChanged uuid  
setVmReady uuid v = saveConfigProperty uuid vmReady (v::Bool)
setVmVkbd uuid v = saveConfigProperty uuid vmVkbd (v::Bool)
setVmVfb uuid v = saveConfigProperty uuid vmVfb (v::Bool)
setVmV4V uuid v = saveConfigProperty uuid vmV4v (v::Bool)
setVmRestrictDisplayDepth uuid v = saveConfigProperty uuid vmRestrictDisplayDepth (v::Bool)
setVmRestrictDisplayRes uuid v = saveConfigProperty uuid vmRestrictDisplayRes (v::Bool)
setVmPreserveOnReboot uuid v = saveConfigProperty uuid vmPreserveOnReboot (v::Bool)
setVmBootSentinel uuid v = saveOrRmConfigProperty uuid vmBootSentinel (v::Maybe String)
setVmHpet uuid v = saveConfigProperty uuid vmHpet (v::Bool)
setVmTimerMode uuid v = saveConfigProperty uuid vmTimerMode (v::String)
setVmNestedHvm uuid v = saveConfigProperty uuid vmNestedHvm (v::Bool)
setVmSerial uuid v = saveConfigProperty uuid vmSerial (v::String)

-- set autolock flag on the vm xenstore tree, per cd device
-- cd devices which have sticky bit are not subject to autolock ever
setVmAutolockCdDrives uuid v =
  whenDomainID_ uuid $ \domid ->
    mapM_ (set domid) =<< devs
  where
    devs = mapM (\d -> (,) <$> pure d <*> getCdDeviceStickyVm d)
            =<< liftIO getHostBSGDevices
    set domid (d,sticky_vm) =
      let sticky = sticky_vm /= Nothing in -- sticky somewhere
      liftIO $ xsWrite (autolockpath domid d) (f $ v && (not sticky))
      where
        f True = "1"
        f _ = "0"
    autolockpath domid (BSGDevice a b c d) =
      printf "/local/domain/%d/bsgdev-req/%s/autolock" domid (printf "%d_%d_%d_%d" a b c d :: String)

-- Create XenStore information used by switcher
exportVmSwitcherInfo :: Uuid -> Rpc ()
exportVmSwitcherInfo uuid =
    do running <- isRunning uuid
       typ     <- getVmType uuid
       typStr  <- readConfigPropertyDef uuid vmType "svm"
       if (not running)
         then liftIO $ xsRm path
         else do name   <- getVmName uuid
                 slot   <- getVmSlot uuid
                 image  <- getVmImagePath uuid
                 hidden <- getVmHiddenInSwitcher uuid
                 hide_switcher <- not <$> getVmShowSwitcher uuid
                 domid  <- fromMaybe (-1) <$> getDomainID uuid
                 seamlessid <- getVmSeamlessId uuid
                 liftIO $ do
                   xsWrite "/xenmgr/vms" ""
                   xsChmod "/xenmgr/vms" "r0"
                   xsWrite path ""
                   xsChmod path "r0"
                   xsWrite (path ++ "/domid") (show domid)
                   xsWriteUTF8 (path ++ "/name" ) name
                   xsWrite (path ++ "/slot" ) (show slot)
                   xsWrite (path ++ "/image") image
                   xsWrite (path ++ "/type" ) typStr
                   xsWrite (path ++ "/hidden") (if hidden then "1" else "0")
                   xsWrite (path ++ "/hide-switcher" ) (if hide_switcher then "1" else "0")
                   xsWrite (path ++ "/seamlessid") seamlessid
                   case typ of
                     ServiceVm tag -> xsWrite (path ++ "/" ++ tag) "1"
                     _ -> return ()
 where
    path = "/xenmgr/vms/" ++ show uuid
