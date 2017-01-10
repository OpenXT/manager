--
-- Copyright (c) 2013 Citrix Systems, Inc.
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

{-# LANGUAGE OverloadedStrings #-}
module XenMgr.Connect.Xenvm
             (
             -- Queries
               domainID
             , stubDomainID
             , domainXsPath
             --, isRunning  --disable this export for now, as xl also implements and exports this function
             , isFocused
             , state
             , stateStr
             , acpiState
             , wiredMac
             , nics
             , requiredToBootKib

             -- Xenvm activities
             , forkXenvm
             , quitXenvm
             , isXenvmUp
             , readConfig
             , onNotify
             , onNotifyRemove

             -- VM activities
             , start, startPaused
             , reboot
             , shutdown
             , destroy
             , waitForState
             , waitForAcpiState
             , sleep
             , hibernate
             , resumeFromSleep
             , suspendToFile
             , resumeFromFile
             , pause
             , unpause

             , changeCd
             , changeNicNetwork
             , connectVif
             , setMemTarget
             , setNicBackendDom

             , NotifyHandler
             ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent

import Data.String
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

import Tools.XenStore
import Tools.Log
import Tools.Process
import Tools.Misc
import Vm.Types
import Vm.State
import XenMgr.Rpc
import Control.Monad.Error (catchError, throwError)

type Params = [(String,String)]

domainID :: MonadRpc e m => Uuid -> m (Maybe DomainID)
domainID uuid = catchNoService Nothing $ do
    r <- invoke uuid "get_domid" []
    let id = fromInteger (read $ unpackStrArg r)
    return $ if id<0 then Nothing else Just id

stubDomainID :: MonadRpc e m => Uuid -> m (Maybe DomainID)
stubDomainID uuid = catchNoService Nothing $ do
    r <- invoke uuid "get_stubdomid" []
    let id = fromInteger (read $ unpackStrArg r)
    return $ if id<0 then Nothing else Just id

domainXsPath :: MonadRpc e m => Uuid -> m String
domainXsPath uuid = local `fmap` domainID uuid where
    local Nothing = "/local/domain/unknown"
    local (Just domid) = "/local/domain/" ++ show domid

isRunning :: (MonadRpc e m) => Uuid -> m Bool
isRunning uuid = (not . (`elem` [Shutdown, Rebooted])) `fmap` state uuid

isFocused :: MonadRpc e m => Uuid -> m Bool
isFocused uuid = catchNoService False $ do
    s <- state uuid
    p <- domainXsPath uuid
    haveFocus s p
  where
   -- If we are not running, we don't have focus,
   -- but if we are running, we test the xenstore node
   haveFocus Shutdown _    = return False
   haveFocus _        domP = let path = domP ++ "/switcher/have_focus" in
                             liftIO $
                                    xsRead path >>=
                                    -- We assumed not focused if the xenstore node cannot be read
                                    return . maybe False (== "1")

state :: MonadRpc e m => Uuid -> m VmState
state uuid = catchNoService Shutdown $ do
    r <- invoke uuid "get_status" []
    return $ stateFromStr (unpackStrArg r)

--FIXME: broadside misery, remove this on master
stateStr :: Uuid -> Rpc String
stateStr uuid = catchNoService "shutdown" $ do
    r <- invoke uuid "get_status" []
    return $ unpackStrArg r

acpiState :: Uuid -> Rpc AcpiState
acpiState uuid = catchNoService 5 $ do
  r <- invoke uuid "get_acpi_state" []
  return $ read (unpackStrArg r)

wiredMac :: Uuid -> Rpc (Maybe String)
wiredMac uuid = wiredNic uuid >>= return . (maybe Nothing $ Just . nicMac)

-- first nic is assumed to be wired
-- TODO: lift this assumption
wiredNic :: Uuid -> Rpc (Maybe Nic)
wiredNic uuid = catchNoService Nothing $
    do ns <- nics uuid
       case ns of
         []    -> return Nothing
         n : _ -> return $ Just n

nics :: Uuid -> Rpc [Nic]
nics uuid = catchNoService [] $ do
    r <- invoke uuid "nic_list" []
    let text         = T.pack $ unpackStrArg r
        descriptions = tail $ T.lines text
    return $ map parse descriptions
  where
    parse str = let columns  = T.split delim str
                    columns' = map T.strip columns
                in
                  case columns' of
                    [id, bridge, mac, _] ->
                        Nic (XbDeviceID $ read $ T.unpack id) (networkFromStr $ T.unpack bridge) (T.unpack mac)
                    _ ->
                        error "unexpected nic list string from xenvm"
    delim = T.pack "|"

requiredToBootKib :: Uuid -> Rpc Integer
requiredToBootKib uuid =
  read . unpackStrArg <$> invoke uuid "get" [("field","required-to-boot-kib")]

-- Start XENVM and wait until it's dbus interface is ready
-- TODO: clenup somehow ?
forkXenvm :: Uuid -> Rpc ()
forkXenvm uuid =
    do dbusUp <- liftIO $ newEmptyMVar
       onNotify uuid "dbus-rpc-up" (whenDbusRpcUp dbusUp)
       output <- liftIO $ spawnShell' ("xenvm --config " ++ configPath)
       case output of
         Nothing ->
             -- Exit code was <> 0
             -- If exit code <> 0, this usually means that an instance of xenvm is already running,
             -- so we do not have to wait for dbus ready message
             do info $ "Xenvm " ++ show uuid ++ " already running, skipping monitor start."
                return ()
         Just _ ->
             -- Wait until DBUS rpc is up
             do debug "waiting until xenvm RPC is ready"
                liftIO $ takeMVar dbusUp
                return ()
  where
    configPath = "/tmp/xenmgr-xenvm-" ++ show uuid
    whenDbusRpcUp dbusUp args =
        do debug "detected xenvm RPC up event"
           liftIO $ putMVar dbusUp True


quitXenvm :: Uuid -> Rpc ()
quitXenvm uuid = void (invoke uuid "quit" []) `catchError` noop
    where noop _ = return ()

isXenvmUp :: Uuid -> Rpc Bool
isXenvmUp uuid = serviceNameTaken (xenvmServiceName uuid)

readConfig :: Uuid -> Rpc ()
readConfig uuid = do
  invoke uuid "read_config" []
  return ()

start :: Uuid -> Rpc ()
start uuid = do
  invoke uuid "start" []
  return ()

startPaused :: Uuid -> Rpc ()
startPaused uuid = do
  invoke uuid "start" [ ("paused", "true") ]
  return ()

pause :: Uuid -> Rpc ()
pause uuid = do
  invoke uuid "pause" []
  return ()

unpause :: Uuid -> Rpc ()
unpause uuid = do
  invoke uuid "unpause" []
  return ()

reboot :: Uuid -> Rpc ()
reboot uuid = do
  invoke uuid "reboot" []
  return ()

shutdown :: Uuid -> Rpc ()
shutdown uuid = do
  r <- isRunning uuid
  case r of True -> invoke uuid "halt" []
            _    -> return []
  return ()

destroy :: Uuid -> Rpc ()
destroy uuid = do
  invoke uuid "destroy" []
  return ()

sleep :: Uuid -> Rpc ()
sleep uuid = do
  invoke uuid "s3suspend" [ ("timeout", "90") ]
  return ()

hibernate :: Uuid -> Rpc ()
hibernate uuid = do
  invoke uuid "s4suspend" [ ("timeout", "90") ]
  return ()

resumeFromSleep :: Uuid -> Rpc Bool
resumeFromSleep uuid = catchNoService False $ do
  invoke uuid "trigger" [ ("params", "s3resume") ]
  -- max 10s wait
  waitForAcpiState uuid 0 (Just 10)

suspendToFile :: Uuid -> FilePath -> Rpc ()
suspendToFile uuid file = do
  invoke uuid "suspend" [ ("file", file) ]
  return ()

resumeFromFile :: Uuid -> FilePath -> Bool -> Bool -> Rpc ()
resumeFromFile uuid file delete_file paused = do
  let delete_str = if delete_file then "true" else "false"
      paused_str = if paused then "true" else "false"
  invoke uuid "restore" [ ("file", file), ("delete", delete_str), ("paused", paused_str) ]
  return ()

waitForState :: Uuid -> VmState -> Maybe Int -> Rpc Bool
waitForState uuid expected timeout = do
    s <- state uuid
    case (s, timeout) of
      -- got right state, exit
      (x, _)       | x == expected -> return True

      -- we timed out while waiting for state
      (_, Just t)  | t <= 0        -> return False

      -- we continue waiting with lesser timeout
      (_, Just t)  | t >  0        -> do liftIO (threadDelay $ 10^6)
                                         waitForState uuid expected (Just $ t-1)

      -- we have no timeout, wait indifinitely
      (_, Nothing)                 -> liftIO (threadDelay $ 10^6) >> waitForState uuid expected Nothing
      _                            -> error "impossible"

waitForAcpiState :: Uuid -> Int -> Maybe Int -> Rpc Bool
waitForAcpiState uuid expected timeout = do
    s <- acpiState uuid
    case (s, timeout) of
      -- got right state, exit
      (x, _)       | x == expected -> return True

      -- we timed out while waiting for state
      (_, Just t)  | t <= 0        -> return False

      -- we continue waiting with lesser timeout
      (_, Just t)  | t >  0        -> do liftIO (threadDelay $ 10^6)
                                         waitForAcpiState uuid expected (Just $ t-1)

      -- we have no timeout, wait indifinitely
      (_, Nothing)                 -> liftIO (threadDelay $ 10^6) >> waitForAcpiState uuid expected Nothing
      _                            -> error "impossible"

changeCd :: Uuid -> String -> Rpc ()
changeCd uuid path = do
  invoke uuid "cd_insert_file" [ ("virtpath", "hdc")
                               , ("physpath", path ) ]
  return ()

--TODO: bridge -> network
changeNicNetwork :: MonadRpc e m => Uuid -> NicID -> Network -> m ()
changeNicNetwork uuid nid@(XbDeviceID nicid) network = do
  invoke uuid "nic_network_switch" [ ("id"    , show nicid)
                                   , ("network", networkToStr network) ]
  return ()

nicFrontendPath :: MonadRpc e m => Uuid -> NicID -> m (Maybe String)
nicFrontendPath uuid (XbDeviceID nicid) =
    do domainP <- domainXsPath uuid
       vifs  <- liftIO . xsDir $ domainP ++ "/device/vif"
       vwifs <- liftIO . xsDir $ domainP ++ "/device/vwif"
       let nicid_str = show nicid
       case () of
         _ | nicid_str `elem` vifs  -> return $ Just (domainP ++ "/device/vif/" ++ nicid_str)
           | nicid_str `elem` vwifs -> return $ Just (domainP ++ "/device/vwif/" ++ nicid_str)
           | otherwise -> return Nothing

-- Connect or disconnect VIF
connectVif :: MonadRpc e m => Uuid -> NicID -> Bool -> m ()
connectVif uuid nicid connect = do
    domainP <- domainXsPath uuid
    front   <- nicFrontendPath uuid nicid
    case front of
      Nothing -> warn $ "failed to lookup nic " ++ show nicid
      Just fp -> do let p = fp ++ "/disconnect"
                    liftIO $ xsWrite p value
  where
    value | connect == True      = "0"
          | otherwise            = "1"

setMemTarget :: Uuid -> Int -> Rpc ()
setMemTarget uuid mbs = do
  invoke uuid "set_mem_target" [ ("mb", show mbs) ]
  return ()

setNicBackendDom :: Uuid -> NicID -> DomainID -> Rpc ()
setNicBackendDom uuid nic domid = do
  invoke uuid "set_nic_backend_dom" [ ("id", show nic)
                                    , ("domid", show domid) ]
  return ()

-- Get a single string result from an RPC call
unpackStrArg :: [Variant] -> String
unpackStrArg r = case fromVariant . head $ r of
                   Just str -> str
                   Nothing  -> error "failed to unpack string arg."

-- A higher level wrapper which packs parameter to format expected by xenvm
invoke :: MonadRpc e m => Uuid -> String -> Params -> m [Variant]
invoke uuid method params =
    rpcXenvm uuid method [toVariant $ pack params]

-- Xenvm expects parameters to be passed as a dictionary mapping strings to strings..
pack :: Params -> M.Map String String
pack = M.fromList

-- Try to call RPC on a proper xenvm instance
rpcXenvm :: MonadRpc e m => Uuid -> String -> [Variant] -> m [Variant]
rpcXenvm uuid method = rpcCallOnce . xenvmcall uuid method

--
type NotifyHandler = [String] -> Rpc ()

onNotifyRemove :: Uuid -> String -> NotifyHandler -> Rpc ()
onNotifyRemove uuid msgname action = 
	let rule = matchSignal "xenvm.signal.notify" "notify"
	in
		rpcOnSignalRemove rule process
  where
    process _ signal =
        let [uuidV, _, statusV] = signalArgs signal
            uuid'  = let Just v = fromVariant $ uuidV in v
            status = let Just v = fromVariant $ statusV in v
            splits = split ':' status
        in
          when (uuid == uuid') $
               case splits of
                 (msg:args) | msg == msgname    -> action args
                 _                              -> return ()
	

-- FIXME: centralise handling of events using one signal handler
onNotify :: Uuid -> String -> NotifyHandler -> Rpc ()
onNotify uuid msgname action =
    let rule = matchSignal "xenvm.signal.notify" "notify"
    in
      rpcOnSignal rule process
  where
    process _ signal =
        let [uuidV, _, statusV] = signalArgs signal
            uuid'  = let Just v = fromVariant $ uuidV in v
            status = let Just v = fromVariant $ statusV in v
            splits = split ':' status
        in
          when (uuid == uuid') $
               case splits of
                 (msg:args) | msg == msgname    -> action args
                 _                              -> return ()

xenvmServiceName :: Uuid -> String
xenvmServiceName uuid = "org.xen.vm.uuid_" ++ uuid' where
    uuid' = uuidStrUnderscore uuid

xenvmcall :: Uuid -> String -> [Variant] -> RpcCall
xenvmcall uuid memb args =
  RpcCall service object interface (fromString memb) args
  where
    service   = fromString $ "org.xen.vm.uuid_" ++ uuid'
    -- Interface name of xenvm object is a bit dodgy, should probably change it to not include UUID at some point
    interface = fromString $ "org.xen.vm.uuid_" ++ uuid'
    object    = fromString $ "/org/xen/vm/" ++ uuid'
    uuid'     = uuidStrUnderscore uuid

catchNoService :: MonadRpc e m => a -> m a -> m a
catchNoService def f = f `catchError` badthings where
    badthings e
        = case remoteErrorName `fmap` toRemoteErr e of
            Just "org.freedesktop.DBus.Error.ServiceUnknown" -> return def
            _ -> throwError e


