--
-- This module provides an implementation to interface with
-- libXL, a flavor of toolstack for the Xen Hypervisor.
-- Most calls here simply invoke the xl command line utility
-- for simplicity, although one could bypass the xl command line
-- utility by using the Haskell FFI (foreign function interface)
-- to hook directly into libXL if more robust features are desired.
--
-- Author: Chris Rogers <rogersc@ainfosec.com>
--


{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module XenMgr.Connect.Xl
    (
    --xl domain control
      start
    , shutdown
    , unpause
    , pause
    , destroy
    , resumeFromSleep
    , reboot
    , sleep
    , hibernate
    , suspendToFile
    , resumeFromFile
    , changeCd
    , setMemTarget
    , acpiState
    , waitForAcpiState
    , waitForState

    --xl/toolstack queries
    , domainID
    , domainXsPath
    , getDomainId
    , isRunning
    , isFocused
    , state

    --dbus stuff
    , onNotify
    , onNotifyRemove
    , xlSurfmanDbus
    , xlInputDbus
    , setNicBackendDom
    , removeNic
    , addNic
    , connectVif
    , changeNicNetwork
    , wakeIfS3
    ) where

import Control.Exception as E
import Control.Applicative
import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Concurrent
import Data.String
import Data.List as L
import Data.Typeable
import Data.Text as T
import Data.Maybe
import Vm.Types
import Vm.DmTypes
import Vm.State
import Tools.Misc as TM
import Tools.XenStore
import Tools.Log
import System
import System.Cmd
import System.Process
import System.Directory
import System.IO
import XenMgr.Rpc
import XenMgr.Db
import XenMgr.Errors
import XenMgr.Connect.NetworkDaemon
import qualified Data.Map as M
import Text.Printf

type NotifyHandler = [String] -> Rpc ()
type Params = [(String, String)]

data XlExceptionClass = XlException String
    deriving (Typeable)
instance Exception XlExceptionClass

instance Show XlExceptionClass where
    show e@(XlException s) = show s

bailIfError :: ExitCode -> String -> IO ()
bailIfError exitCode msg =
    do
      case exitCode of
        ExitSuccess -> return ()
        _           -> throw $ XlException msg

resumeFromSleep :: Uuid -> IO Bool
resumeFromSleep uuid = do
    domid <- getDomainId uuid
    exitCode <- system ("xl trigger " ++ domid ++ " s3resume")
    case exitCode of
      _  -> waitForAcpiState uuid 0 (Just 10)

--Given the uuid, get the domid, if there is one
domainID :: Uuid -> IO (Maybe DomainID)
domainID uuid = do
    domid  <- getDomainId uuid
    return $ if domid == "" then Nothing else Just (read domid :: DomainID)

--Wait the provided duration for the domain to get into the specified acpi state
waitForAcpiState :: Uuid -> Int -> Maybe Int -> IO Bool
waitForAcpiState uuid expected timeout = do
    s <- acpiState uuid
    case  (s, timeout) of
      (x, _)          | x == expected -> return True
      (_, Just t)     | t <= 0        -> return False
      (_, Just t)     | t > 0         -> do liftIO (threadDelay $ 10^6)
                                            waitForAcpiState uuid expected (Just $ t-1)
      (_, Nothing)                    -> liftIO (threadDelay $ 10^6) >> waitForAcpiState uuid expected Nothing
      _                               -> error "impossible"

--We need to concoct acpi states (5 or 0) for fully PV domains
--since get_hvm_param doesn't apply
acpiState :: Uuid -> IO AcpiState
acpiState uuid = do
    domid <- getDomainId uuid
    case domid of
      "" -> return 5 --no domid indicates domain is off, so acpi state 5
      _  -> do
              acpi_state <- readProcess "xl" ["acpi-state", domid] []
              let plain_acpi = (T.unpack (T.stripEnd (T.pack acpi_state)))
              case plain_acpi of
                "-1" -> return 0  --If we have the domid but xl returns us -1 for acpi state, it's likely the domain
                                  --is fully PV, so just return 0.
                _    -> return $ (read plain_acpi :: Int)

--Return whether the vm is currently in focus
isFocused :: Uuid -> IO Bool
isFocused uuid = do
    s <- state uuid
    p <- domainXsPath uuid
    haveFocus s p
  where
    haveFocus Shutdown _    = return False
    haveFocus _        domP = let path = domP ++ "/switcher/have_focus" in
                              liftIO $ xsRead path >>= return . maybe False (== "1")

--Return the path to the domain in the xenstore
domainXsPath :: Uuid -> IO String
domainXsPath uuid = do
    domid <- getDomainId uuid
    case domid of
      "" -> return $ "/local/domain/unknown"
      _  -> return $ "/local/domain/" ++ domid


--The following functions are all domain lifecycle operations, and self-explanatory

reboot :: Uuid -> IO ()
reboot uuid =
    do
      domid <- getDomainId uuid
      exitCode <- system ("xl reboot " ++ domid)
      case exitCode of
        ExitSuccess   -> return ()
        _             -> do _ <- system ("xl reboot -F " ++ domid)
                            return ()

shutdown :: Uuid -> IO ()
shutdown uuid =
    do
      domid <- getDomainId uuid
      exitCode  <- system ("xl shutdown " ++ domid)
      case exitCode of
        ExitSuccess   -> return ()
        _             -> do _ <- system ("xl shutdown -F " ++ domid)
                            return ()

pause :: Uuid -> IO ()
pause uuid =
    do
      domid     <- getDomainId uuid
      exitCode  <- system ("xl pause " ++ domid)
      bailIfError exitCode "Error parsing domain."

unpause :: Uuid -> IO ()
unpause uuid = do
    domid <- getDomainId uuid
    case domid of
        "" -> return ()
        _  -> do
                exitCode <- system ("xl unpause " ++ domid)
                bailIfError exitCode "Error unpausing domain."

--It should be noted that by design, we start our domains paused to ensure all the
--backend components are created and xenstore nodes are written before the domain
--begins running.
start :: Uuid -> IO ()
start uuid =
    do
      state <- state uuid
      case state of
        Shutdown -> do
                      (_, _, Just err, handle) <- createProcess (proc "xl" ["create", configPath uuid, "-p"]){std_err = CreatePipe}
                      ec <- waitForProcess handle
                      stderr <- hGetContents err
                      case ec of
                        ExitSuccess -> return ()
                        _           -> throw $ XlException $ L.intercalate "<br>" $ L.lines stderr
        _        -> do return ()

--if domain has no domid, the domain is already dead. But we should make sure
--the xenstore state is set to 'shutdown'.  Sometimes when domains crash on startup,
--UI shows either 'starting' or 'off', but the internal state is 'creating-devices',
--preventing further boots
destroy :: Uuid -> IO ()
destroy uuid = do
    domid    <- getDomainId uuid
    case domid of
        ""  -> do maybe_state <- xsRead ("/state/" ++ show uuid ++ "/state")
                  case maybe_state of
                    Just state -> if state /= "shutdown" then do xsWrite ("/state/" ++ show uuid ++ "/state") "shutdown" else return ()
                    Nothing    -> return ()
        _   -> do exitCode <- system ("xl destroy " ++ domid)
                  bailIfError exitCode "Error destroying domain."

sleep :: Uuid -> IO ()
sleep uuid =
    do
      domid    <- getDomainId uuid
      exitCode <- system ("xl trigger " ++ domid ++ " sleep")
      bailIfError exitCode "Error entering s3."

hibernate :: Uuid -> IO ()
hibernate uuid =
    do
      domid    <- getDomainId uuid
      exitCode <- system ("xl hiberate " ++ domid)
      bailIfError exitCode "Error entering s4."
      
suspendToFile :: Uuid -> FilePath -> IO ()
suspendToFile uuid file =
    do
      domid    <- getDomainId uuid
      exitCode <- system ("xl save " ++ domid ++ " " ++ file)
      bailIfError exitCode "Error suspending to file."

resumeFromFile :: Uuid -> FilePath -> Bool -> Bool -> IO ()
resumeFromFile uuid file delete paused =
    do
      let p = if paused then "-p" else ""
      _ <- system ("xl restore " ++ p ++ file)
      if delete then removeFile file else return ()

--Ask xl directly for the domid
getDomainId :: Uuid -> IO String
getDomainId uuid = do
    domid <- readProcess "xl" ["uuid-to-domid", show uuid] []
    let plain_domid = (T.unpack (T.stripEnd (T.pack domid)))
    case plain_domid of
      "-1" -> return ("")
      _    -> return (plain_domid) --remove trailing newline

--For a given uuid, change the iso in the cd drive slot
changeCd :: Uuid -> String -> IO ()
changeCd uuid path = do
    domid <- getDomainId uuid
    (exitCode, _, _)  <- readProcessWithExitCode "xl" ["cd-insert", domid, "hdc", path] []
    bailIfError exitCode "Error changing cd."

--Return the frontend xenstore path of the nic device (or Nothing)
nicFrontendPath :: Uuid -> NicID -> IO (Maybe String)
nicFrontendPath uuid (XbDeviceID nicid) =
    do domainP <- domainXsPath uuid
       vifs   <- liftIO . xsDir $ domainP ++ "/device/vif"
       vwifs  <- liftIO . xsDir $ domainP ++ "/device/vwif"
       let nicid_str = show nicid
       case () of
         _ | nicid_str `elem` vifs -> return $ Just (domainP ++ "/device/vif/" ++ nicid_str)
           | nicid_str `elem` vwifs -> return $ Just (domainP ++ "/device/vwif/" ++ nicid_str)
           | otherwise -> return Nothing

--For a given nic, reassign the backend network it should belong to
changeNicNetwork :: Uuid -> NicID -> Network -> IO ()
changeNicNetwork uuid nid@(XbDeviceID nicid) network = do
    domid <- getDomainId uuid
    domainP <- domainXsPath uuid
    backendPath <- xsRead (domainP ++ "/device/vif/" ++ show nicid ++ "/backend")
    case backendPath of
        Just b  -> do xsWrite (b ++ "/bridge") (show network)
                      return ()
        Nothing -> return ()

--Ask a vif to switch to the connected or disconnected state
connectVif :: Uuid -> NicID -> Bool -> IO ()
connectVif uuid nicid connect = do
    domainP <- domainXsPath uuid
    front   <- nicFrontendPath uuid nicid
    case front of
        Nothing -> warn $ "failed to lookup nic " ++ show nicid
        Just fp -> do let p = fp ++ "/disconnect"
                      liftIO $ xsWrite p value
  where
    value | connect == True         = "0"
          | otherwise               = "1"

-- Check if domain is in S3, if so, wake it up
wakeIfS3 :: Uuid -> IO ()
wakeIfS3 uuid = do
    acpi_state <- acpiState uuid
    case acpi_state of
        3 -> do resumeFromSleep uuid
                return ()
        _ -> return ()

--Adjust memory through the balloon driver, unreliable, requires correct
--paravirt drivers.  Implemented here in the event ballooning is ever desired
--and implemented correctly
setMemTarget :: Uuid -> Int -> IO ()
setMemTarget uuid mbs = do
    domid    <- getDomainId uuid
    exitCode <- system ("xl mem-set " ++ domid ++ " " ++ show mbs ++ "m")
    bailIfError exitCode "Error setting mem target."

removeNic :: Uuid -> NicID -> DomainID -> IO ()
removeNic uuid nic back_domid = do
    domid <- getDomainId uuid
    system ("xl network-detach " ++ domid ++ " " ++ show nic)
    return ()

addNic :: Uuid -> NicID -> String -> DomainID -> IO ()
addNic uuid nic net back_domid = do
    domid <- getDomainId uuid
    stubdomid <- (liftIO $ xsRead ("/xenmgr/vms/" ++ show uuid ++ "/stubdomid"))
    let typ = isJust stubdomid
    let wireless = L.isInfixOf "wifi" net
    (ec,stdout,_)<- readProcessWithExitCode "xl" ["network-attach", domid, printf "bridge=%s" net, printf "backend=%s" (show back_domid),
            if typ then "type=ioemu" else "type=vif", if wireless then "wireless=1" else "wireless=0", printf "devid=%s" (show nic)] []
    return ()

--Given the uuid of a domain and a nic id, set the target backend domid for that nic
setNicBackendDom :: Uuid -> NicID -> DomainID -> IO ()
setNicBackendDom uuid nic back_domid = do
    domid    <- getDomainId uuid
    exitCode <- system ("xl network-detach " ++ show domid ++ " " ++ show nic)
    bailIfError exitCode "Error detatching nic from domain."
    exitCode <- system ("xl network-attach " ++ domid ++ " backend=" ++ show back_domid)
    bailIfError exitCode "Error attaching new nic to domain."

--Implement signal watcher to fire off handler upon receiving
--notify message over dbus
onNotify :: Uuid -> String -> NotifyHandler -> Rpc ()
onNotify uuid msgname action =
    let rule = matchSignal "com.citrix.xenclient.xenmgr" "notify"
    in
      rpcOnSignal rule process
  where
    process _ signal =
        let [uuidV, statusV] = signalArgs signal
            uuid'  = let Just v = fromVariant $ uuidV in v
            status = let Just v = fromVariant $ statusV in v
            splits = TM.split ':' status
        in
          when (uuid == uuid') $
               case splits of
                 (msg:args) | msg == msgname    -> action args
                 _                              -> return ()

--Remove the handler setup by onNotify
onNotifyRemove :: Uuid -> String -> NotifyHandler -> Rpc ()
onNotifyRemove uuid msgname action =
    let rule = matchSignal "com.citrix.xenclient.xenmgr" "notify"
    in
        rpcOnSignalRemove rule process
  where
    process _ signal =
        let [uuidV, statusV] = signalArgs signal
            uuid'  = let Just v = fromVariant $ uuidV in v
            status = let Just v = fromVariant $ statusV in v
            splits = TM.split ':' status
        in
          when (uuid == uuid') $
               case splits of
                 (msg:args) | msg == msgname    -> action args
                 _                              -> return ()

--Construct an RPC message for surfman given an argument list and
--command to run.
xlSurfmanDbus uuid memb args =
  RpcCall service object interface (fromString memb) args
  where
    service = fromString $ "com.citrix.xenclient.surfman"
    interface = fromString $ "com.citrix.xenclient.surfman"
    object = fromString $ "/"

--Construct an RPC for inputserver given an argument list and
--command to run
xlInputDbus uuid memb args =
  RpcCall service object interface (fromString memb) args
  where
    service = fromString $ "com.citrix.xenclient.input"
    interface = fromString $ "com.citrix.xenclient.input"
    object = fromString $ "/"

--Path to the xl config file generated on domain creation
configPath uuid = "/tmp/xenmgr-xl-" ++ show uuid
stubConfigPath uuid = "/tmp/xenmgr-xl-" ++ show uuid ++ "-dm"

--Check the domain state to see if the domain is running
isRunning :: (MonadRpc e m) => Uuid -> m Bool
isRunning uuid = (not . (`elem` [Shutdown, Rebooted])) `fmap` (liftIO $ state uuid)

--Xl will write any state updates to the xenstore node "/state/<uuid>/state"
--It's up to xenmgr to setup and maintain a watch on this node to detect state changes
state :: Uuid -> IO VmState
state uuid =
    do
        maybe_state <- xsRead ("/state/" ++ show uuid ++ "/state")
        case maybe_state of
          Just state -> do
                          info $ "active vms, state = " ++ show state
                          return $ stateFromStr state
          Nothing    -> return $ stateFromStr "shutdown"

--Wait for provided duration for the domain to reach a specific state,
--returning false if that never happens.
waitForState :: Uuid -> VmState -> Maybe Int -> IO Bool
waitForState uuid expected timeout = do
    s <- state uuid
    case (s, timeout) of
      -- got right state, exit
      (x, _)       | x == expected -> return True

      -- we timed out while waiting for state
      (_, Just t)  | t <= 0        -> return False

      -- we continue waiting with lesser timeout
      (_, Just t)  | t >  0        -> do (threadDelay $ 10^6)
                                         waitForState uuid expected (Just $ t-1)

      -- we have no timeout, wait indifinitely
      (_, Nothing)                 -> (threadDelay $ 10^6) >> waitForState uuid expected Nothing
      _                            -> error "impossible"
