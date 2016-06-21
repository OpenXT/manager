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

{-# LANGUAGE ScopedTypeVariables,PatternGuards #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Applicative
import Data.String
import Data.List
import Data.Ord
import Data.Maybe
import System
import System.Posix.Files
import System.Posix.Syslog
import System.Posix.Process
import System.Posix.Signals
import System.Console.GetOpt
import System.IO
import System.Timeout (timeout)
import System.Process
import System.FilePath
import Directory

import qualified Tools.Periodic as Periodic
import Tools.Periodic
import Tools.XenStore
import Tools.Misc
import Tools.Log
import Tools.Process
import Tools.Future
import Tools.IfM
import Tools.File

import Vm.Templates
import qualified Vm.V4VFirewall as Firewall
import Vm.React
import XenMgr.Db
import XenMgr.Config
import XenMgr.PowerManagement
import XenMgr.Vm
import XenMgr.Host
import XenMgr.HostOps
import XenMgr.XM
import XenMgr.Rpc
import XenMgr.Connect.InputDaemon
import XenMgr.Connect.GuestRpcAgent
import XenMgr.Testing
import qualified XenMgr.Expose.XenmgrObject
import qualified XenMgr.Expose.VmObject
import qualified XenMgr.Expose.HostObject

import Vm.Monad

-- We need these services in order to start xenmgr at all
prerequisites :: [String]
prerequisites =
    [
     "com.citrix.xenclient.db"
    ]

waitPrerequisites :: Rpc ()
waitPrerequisites = mapM_ wait prerequisites
    where wait s =
              do info $ "waiting for " ++ show s ++ "..."
                 rpcWaitForService s
                 info $ "DONE waiting for " ++ show s ++ "!"

-- Detect PV addons installation, toggle option in vm configuration when detected
-- Check portica status, persist in vm configuration
checkPorticaStatus :: RpcContext -> IO ()
checkPorticaStatus context = forkIO watcher >> return ()
  where
    watcher = do xsWaitFor "/xc_tools/porticaStatus" (rpc context check >> return False)
                 watcher
    -- only check svms, pvms
    check        = debug "testing portica installation.." >> getGuestVms >>= mapM checkVm
    checkVm uuid = isRunning uuid >>= \r -> when r (checkPortica uuid)
    checkPortica uuid = do
      info "detected portica status change"
      installedStr <- liftIO $ xsRead ("/xc_tools/porticaStatus/" ++ show uuid ++ "/installed")
      enabledStr   <- liftIO $ xsRead ("/xc_tools/porticaStatus/" ++ show uuid ++ "/enabled")
      let status = PorticaStatus { porticaInstalled = installedStr == Just "1"
                                 , porticaEnabled   = enabledStr   == Just "1" }
      saveConfigProperty uuid vmPorticaStatus status

autoStartVms :: XM ()
autoStartVms = do
    autostarter False
    return ()
  where
    getStartCandidates = filterM ok where
        ok vm = (&&) <$> liftIO isInstallCompleted
                     <*> liftRpc (not <$> isLoginRequired vm)
    autostarter switched = do
      uuids      <- getStartCandidates =<< dbReadWithDefault [] "/xenmgr/autostart-pending-vms"
      restoreVm  <- liftRpc pmGetScreenRestoreVm
      liftRpc pmClearScreenRestoreVm
      switched <- foldM (start restoreVm uuids) switched uuids
      when (not switched) $ void $ foldM trySwitch False uuids

    trySwitch True  _  = return True
    trySwitch False vm = liftRpc $ switchIfNoService vm

    switchIfNoService uuid = ifM (not <$> isServiceVm uuid) (switchVm uuid) (return False)
    -- start autostart vm, switch if its the first one
    start :: Maybe Uuid -> [Uuid] -> Bool -> Uuid -> XM Bool
    start restoreVm candidates switched uuid =
        ( do delay <- autoStart uuid
             switched' <-
               case () of
                 _ | switched -> return True
                   | restoreVm == Just uuid -> liftRpc $ switchIfNoService uuid
                   | restoreVm == Nothing && uuid `elem` candidates -> liftRpc $ switchIfNoService uuid
                   | otherwise -> return False
             liftIO $ threadDelay (delay * (10^6))
             return switched' )
        `catchError` (\e -> do warn $ "Problem during autostart of " ++ show uuid ++ ": " ++ show e
                               return False )

removeFromAutoStartQueue :: Uuid -> Rpc ()
removeFromAutoStartQueue uuid =
    dbWrite "/xenmgr/autostart-pending-vms" =<<
    delete uuid <$>
    dbReadWithDefault [] "/xenmgr/autostart-pending-vms"

autoStart :: Uuid -> XM Int
autoStart uuid = do
    liftRpc $ removeFromAutoStartQueue uuid
    -- it might be already running because of dehibernation code
    running <- liftRpc $ isRunning uuid
    if (not running)
      then do info $ "AutoStart of " ++ show uuid
              startVm uuid `catchError` \e -> warn $ "Problem trying to autostart " ++ show uuid ++ ": " ++ show e
              typ <- liftRpc $ getVmGraphics uuid
              case typ of
                HDX -> liftRpc appPvmAutoStartDelay
                _   -> liftRpc appSvmAutoStartDelay
      else return 0

isServiceVm uuid  = getVmType uuid >>= return . test where
  test (ServiceVm _) = True
  test _ = False

-- We write the uuids of VMS requiring autostart to /xemgr/autostart-pending-vms key
setupAutoStart :: XM ()
setupAutoStart = do
    hibernated <- liftRpc platformWasHibernated
    dbRm "/platform/hibernated"
    uuids <- liftRpc $
      if not hibernated
         then getVms >>= filterM getVmStartOnBoot
         else do
           hibs   <- getVmsToDehibernate
           suuids <- getVms >>= filterM isServiceVm >>= filterM getVmStartOnBoot
           return . nub $ suuids ++ hibs
    when hibernated $ 
      info $ "coming out of hibernation, vms to start: " ++ show uuids
    setupAutoStartUuids uuids
  where
    platformWasHibernated =
        dbMaybeRead "/platform/hibernated" >>= return . test where
                  test (Just "true") = True
                  test _ = False

setupAutoStartUuids :: [Uuid] -> XM ()
setupAutoStartUuids uuids = do
    autoStart <- liftRpc appAutoStart
    when autoStart $
         return uuids
         >>= filterM (liftRpc . getVmReady)
         >>= filterM testStartServiceVm
         >>= liftRpc . priorityOrdering
         >>= serialise

    -- do autostart queue depletion when you detect authentication success
    xm <- xmContext
    liftRpc $ rpcOnSignal auth_status_rule (\_ sig -> runXM xm autoStartVms)

    -- also deplete the queue now
    autoStartVms
    return ()
  where
    auth_status_rule  = matchSignal "com.citrix.xenclient.input" "auth_status"
    serialise uuids   = dbWrite "/xenmgr/autostart-pending-vms" uuids
    testStartServiceVm uuid = liftRpc$ 
      ifM (isServiceVm uuid) (useServiceVm uuid) (return True)

data AutostartVm = AutostartVm { avm_uuid :: Uuid
                               , avm_prio :: Int
                               , avm_slot :: Int
                               , avm_gfx :: VmGraphics }

-- order by (reverse) start on boot priority
-- so that, higher priority gets started EARLIER
-- if priorities are equal, we order by type (pvms first), then by slot
priorityOrdering :: [Uuid] -> Rpc [Uuid]
priorityOrdering uuids =
    do avms <- sortBy priority_compare <$> mapM get_avm uuids
       return $ map avm_uuid avms
    where
      get_avm uuid =
          do prio <- getVmStartOnBootPriority uuid
             slot <- getVmSlot uuid
             gfx  <- getVmGraphics uuid
             return $ AutostartVm { avm_uuid = uuid
                                  , avm_prio = prio
                                  , avm_slot = slot
                                  , avm_gfx = gfx }
      priority_compare a b =
          case () of
            _ | avm_prio a > avm_prio b -> LT -- priority always wins
              | avm_prio a < avm_prio b -> GT
              | avm_gfx a == HDX       -> LT -- pvm wins if priority fails
              | avm_gfx b == HDX       -> GT
              | otherwise               -> compare (avm_slot a) (avm_slot b) -- slot wins in all else fails

-- Run these operations periodically
periodics :: XmContext -> RpcContext -> Periodic.Context
periodics xm_context rpccontext =
    foldl' accumulate Periodic.empty actions
  where
    accumulate acc (period,action) = periodically acc period action
    -- The list of actions we run periodically
    actions =
        [
         (30000, checkStorage),(30000, void $ rpc rpccontext (hostMonitorIdleness xm_context) )-- every 30 secs checks amount of free space on storage partition and idle time of host
        ]

    checkStorage = do
      rpc rpccontext hostCheckFreeStorage
      return ()

hostMonitorIdleness :: XmContext -> Rpc ()
hostMonitorIdleness xm_context = do
    idle_time <- fromIntegral <$> inputGetIdleTime
    idle_time_threshold <- appIdleTimeThreshold
    when (idle_time_threshold /= 0 && idle_time >= idle_time_threshold ) $
      runXM xm_context hostSleep

exposeStuff :: XM ()
exposeStuff = do
    testingCtx <- liftIO $ testingCreateContext
    XenMgr.Expose.XenmgrObject.expose testingCtx
    host_info <- liftIO $ initHostInfo
    XenMgr.Expose.HostObject.expose host_info
    mapM_ XenMgr.Expose.VmObject.expose =<< liftRpc getVms

interceptOtherSignals :: XM ()
interceptOtherSignals = liftRpc $ do
    onXorgRunning $ \uuid -> liftIO $ xsWrite ("/xenmgr/vms/" ++ show uuid ++ "/xorg") "1"

-- Detect and report config problems to log
detectVmsConfigProblems :: Rpc ()
detectVmsConfigProblems = do
    vms <- getConfigCorruptionInfo
    mapM_ logErr vms
  where
    logErr (uuid,msg) =
        info $ "VM " ++ show uuid ++ " has corrupt config: " ++ msg

monitorVms :: XM ()
monitorVms = mapM_ monitorAndReactVm =<< liftRpc getVms

data Flag = NoDaemonize
          | NoAutostart
          | WritePid (Maybe String)
          | OptDebug
          | Help
            deriving Eq

options :: [OptDescr Flag]
options =
    [ Option ""  ["no-daemonize"] (NoArg NoDaemonize) "do not daemonize the process"
    , Option ""  ["no-autostart"] (NoArg NoAutostart) "do not autostart vms"
    , Option ""  ["writepid"] (OptArg WritePid "FILE") "write pid to FILE"
    , Option ""  ["debug"] (NoArg OptDebug) "additional debug info (rpc timings etc)"
    , Option "h" ["help"] (NoArg Help) "print usage information" ]

useServiceVm :: Uuid -> Rpc Bool
useServiceVm uuid = return True

initServiceVms :: XM [Uuid]
initServiceVms = liftRpc $ do
  -- XC-8598 removing unused service vms causes nilfvm upgrade issues
  -- wouldn't be a problem if nilfvm wasn't tied to template, but it unnecessarily is atm
  -- trashUnusedServiceVms
  tags  <- liftIO enumServiceVmTags
  uuids <- priorityOrdering =<< filterM useServiceVm =<< filterM getVmStartOnBoot =<< catMaybes <$> mapM configureServiceVm tags

  -- initial network backend node seems to be necessary IF there's no service domain providing network backend
  -- FIXME: do something about this ugly thing
  net_vm <- getDefaultNetworkBackendVm
  case net_vm of
    Nothing ->
        do liftIO $ xsWrite backendNode "0"
           liftIO $ xsChmod backendNode "r0"

    Just net_uuid | not (net_uuid `elem` uuids) ->
        do liftIO $ xsWrite backendNode "0"
           liftIO $ xsChmod backendNode "r0"
    _ -> return ()
  return uuids

balloonDom0 :: Rpc ()
balloonDom0 = go =<< appGetDom0MemTargetMIB where
    go 0 = return ()
    go t = liftIO $
             do info $ "ballooning dom0 to " ++ show t ++ " MiB"
                xsWrite ("/local/domain/0/memory/target") (show . kib $ t)
                info "ballooning dom0 done"
    kib  = (* 1024)

rmSnapshots :: IO ()
rmSnapshots = mapM_ rm =<< return . filter isSnapshot =<< filesInDir "/storage/disks" where
  isSnapshot p = any (== True) . map (flip isSuffixOf p) $ [".snap", ".snap.tmp.vhd"]
  rm p = info ("removing snapshot " ++ p) >> removeLink p

initXenMgr opts = do
  -- show info about corrupted configs
  -- detectVmsConfigProblems
  liftIO rmSnapshots
  xm_context <- newXmContext
  context    <- rpcGetContext
  runXM xm_context $ do
    -- init uivm, netdrv etc
    info "initialising service vms..."
    service_uuids <- initServiceVms

    -- Start monitoring state of all vms (through xenvm)
    info "monitoring vms..."
    monitorVms

    -- put things on dbus
    info "exposing dbus interfaces..."
    exposeStuff

    fire <- liftRpc appGetV4VFirewall
    when fire $ do
      info "starting v4v firewall"
      liftIO . Firewall.reworkRules . rpc context $ do
                      getVms >>= filterM isRunning >>= mapM_ applyVmFirewallRules
      return ()

    -- possibly early bind to pciback to guard against grabbing the device if some vm requires it later
    info "early bind of pci devices to pciback.."
    liftRpc $ mapM_ bindVmPcis =<< getVms
    liftIO $ system "heimdallr /etc/pci-quirks.json"

    -- Hook the power management events etc
    info "initialising power managment..."
    initPm

    interceptOtherSignals

    -- Clear autostart stuff
    dbRm "/xenmgr/autostart-pending-vms"

    -- balloon down dom0 if configured
    --liftRpc balloonDom0

    -- everyone can access and update bsgdev media states
    liftIO $ do
      xsWrite "/xenclient/bsgdev" ""
      xsChmod "/xenclient/bsgdev" "b0"

    -- Find the vms requiring autostart (unless disabled by command line). Includes vms to dehibernate
    when (not $ NoAutostart `elem` opts) $
      setupAutoStart

    -- begin auth process maybe
    authOnBoot <- liftRpc inputAuthOnBoot `catchError` (\ _ -> return False)
    when authOnBoot ( liftRpc inputRunAuthOnBoot >> return () )
  return xm_context


main :: IO ()
main = do
    args <- getArgs
    let (opts,_,_) = getOpt Permute options args
    debug . ("starting.. uptime: " ++) =<< readFile "/proc/uptime"
    when (OptDebug `elem` opts) $ rpcDebug True
    startup opts $ do
      liftIO hostLicenseInit
      -- bracket rpc server connection to make sure it is stopped on fatal exceptions
      rpcServe "com.citrix.xenclient.xenmgr" $ \rpcContext ->
          (do installHandler sigUSR2 (onSigUsr2 sigUSR2 rpcContext) Nothing
              
              -- We have rpc context, so we move to work in RPC monad onwards on
              status <- rpc rpcContext $ do
                        -- wait until prerequisites are connected to dbus
                        info "waiting for prerequisite services to appear on DBUS.."
                        waitPrerequisites
                        xm_context <- initXenMgr opts

                        -- Handle periodic checks
                        liftIO $ do
                          checkPorticaStatus rpcContext
                          service (periodics xm_context rpcContext)
              case status of
                Left error -> fatal $ "Error during initialisation " ++ show error
                Right _    -> return ()
             ) `Control.Exception.catch` (\(ex :: SomeException) -> fatal $ "Caught error: " ++ show ex)

  where
    startup opts f =
        if Help `elem` opts
          then do prg <- getProgName
                  hPutStrLn stderr (usageInfo prg options)
                  exitWith ExitSuccess
          else if NoDaemonize `elem` opts
                 then do pid <- getProcessID
                         dumpPid opts pid
                         runMe f
                 else do pid <- forkProcess (runMe f)
                         dumpPid opts pid

    runMe f = withSyslog "xenmgr" [] USER f
    dumpPid opts pid =
        case fname opts of
          Just path -> writeFile path $ show pid ++ "\n"
          Nothing   -> return ()
        where
          fname [] = Nothing
          fname ((WritePid p) : xs) = p
          fname (x:xs) = fname xs

    -- When kill signals detected, stop RPC and reraise so we die
    onSigUsr2 sig rpcContext =
        CatchOnce $ do
          info "SIGUSR2: shutting down all the vms"
          rpc rpcContext $ pmShutdownVms False
          info "Shutting down vms done"
          raiseSignal sigTERM
