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

module Vm.Monitor
    ( VmMonitor
    , VmEvent(..), Handler, HandlerID
    , react
    , stopReacting
    , removeDefaultEvents
    , submitVmEvent
    , evalVmEvent
    , newVmMonitor
    , getMonitorError
    , vmStateWatch
    , vmStateSubmit
    )
    where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.IORef
import System.IO

import Tools.XenStore
import Tools.Log
import Tools.Misc
import Tools.Text

import Vm.Types
import Vm.State (stateFromStr)
import Vm.Queries
import Vm.ConfigWriter
import qualified XenMgr.Connect.Xl as Xl
import qualified XenMgr.Connect.GuestRpcAgent as RpcAgent
import XenMgr.Rpc
import XenMgr.Errors

type RtcOffset = String

data VmEvent
   = VmStateChange !VmState
   | VmAcpiStateChange !AcpiState
   | VmRtcChange !RtcOffset
   | VmRpcAgentStart | VmRpcAgentStop
   | VmPvAddonsNodeChange | VmPvAddonsUninstallNodeChange
   | VmBsgDevNodeChange
   | VmMeasurementFailure !FilePath !Integer !Integer
   | VmStateUpdate
   | VmAcpiUpdate
     deriving (Eq,Show)

data VmMonitor
   = VmMonitor { vmm_uuid     :: Uuid
               , vmm_submit   :: VmEvent -> IO ()
               , vmm_events   :: IO [VmEvent]
               , vmm_handlers :: MVar [(HandlerID, Handler)]
               , vmm_lasterr  :: IORef (Maybe XmError)
               }

type HandlerID = Int
type Handler = HandlerID -> VmEvent -> Rpc ()

react :: VmMonitor -> Handler -> Rpc HandlerID
react m handle = liftIO $ modifyMVar (vmm_handlers m) install where
    install hs =
        return ( hs ++ [(handler_id, handle)]
               , handler_id )
        where
          handler_id  = maximum_ . map fst $ hs
          maximum_ [] = 0
          maximum_ xs = maximum xs

stopReacting :: VmMonitor -> HandlerID -> Rpc ()
stopReacting m hid = liftIO $ modifyMVar_ (vmm_handlers m) remove where
    remove = return . filter ((/= hid) . fst)

evalVmEvent :: VmMonitor -> VmEvent -> Rpc ()
evalVmEvent m e = do
  ctx <- rpcGetContext
  liftIO $ withMVar (vmm_handlers m)
         $ mapM_ (runHandler ctx e)
  where
    runHandler ctx e (hid,handle) = status (vmm_lasterr m) =<< rpc ctx (handle hid e)
    status err_mv (Left ex) = warn (show ex) >> writeIORef err_mv (Just ex)
    status _ _ = return ()

getMonitorError :: VmMonitor -> Rpc (Maybe XmError)
getMonitorError m = liftIO (readIORef $ vmm_lasterr m)

newVmMonitor :: Uuid -> Rpc VmMonitor
newVmMonitor uuid =
    do evch <- liftIO newChan
       let submit = \x -> liftIO (writeChan evch x)
           events = liftIO (getChanContents evch)
       handlers <- liftIO (newMVar [])
       lasterr <- liftIO (newIORef Nothing)
       let m = VmMonitor {
                    vmm_uuid = uuid
                  , vmm_submit = submit
                  , vmm_events = events
                  , vmm_handlers = handlers
                  , vmm_lasterr = lasterr
                  }
       -- attach event insertion + event loop
       insertDefaultEvents m
       ctx <- rpcGetContext
       liftIO . forkIO $ status =<< rpc ctx (evloop m)
       return m
      where
        status (Left ex) = warn $ "ERROR in evloop for " ++ show uuid ++ ": " ++ (show ex)
        status _ = return ()

submitVmEvent :: VmMonitor -> VmEvent -> Rpc ()
submitVmEvent m e = liftIO $ (vmm_submit m) e

insertDefaultEvents :: VmMonitor -> Rpc ()
insertDefaultEvents m = let uuid = vmm_uuid m in do
    Xl.onNotify uuid "rtc" whenRtc
    Xl.onNotify uuid "vm" whenVm
    Xl.onNotify uuid "power-state" whenPowerState
    RpcAgent.onAgentStarted uuid (submit VmRpcAgentStart)
    RpcAgent.onAgentUninstalled uuid (submit VmRpcAgentStop)

    -- xenstore watches installed on domain creation/shutdown
    watches <- liftIO . sequence $ watchesForVm (vmm_submit m)
    insertWatchEvents m watches

  where
    submit e = liftIO (vmm_submit m e)
    whenRtc (offset:_) = submit (VmRtcChange offset)
    whenRtc _ = return ()

    whenVm ("state":state_str:_) =
        let state = stateFromStr state_str in
        submit (VmStateChange state)
    whenVm _ = return ()

    whenPowerState (state_str:_) =
        case maybeRead state_str of
          Just state -> submit (VmAcpiStateChange state)
          _ -> return ()
    whenPowerState _ = return ()

--Chain some calls to eventually invoke removeMatch
removeDefaultEvents :: Uuid -> Rpc ()
removeDefaultEvents uuid = do
    Xl.onNotifyRemove uuid "rtc" whenRtc
    Xl.onNotifyRemove uuid "vm" whenVm
    Xl.onNotifyRemove uuid "power-state" whenPowerState

    --Need to undo the onAgent stuff to remove match rules
    RpcAgent.onAgentStartedRemove uuid (submit VmRpcAgentStart)
    RpcAgent.onAgentUninstalledRemove uuid (submit VmRpcAgentStop)

  where     --do nothing here on these handlers
    submit _ = return ()
    whenRtc _ = return ()
    whenVm _ = return ()
    whenPowerState _ = return ()

-- add watches on vm creation (or if already running), prune watches on vm destroy
insertWatchEvents :: VmMonitor -> [VmWatch] -> Rpc ()
insertWatchEvents m watches = do
    -- if the vm is already running, add watches
    running <- isRunning uuid
    when running $ addWatches uuid watches
    react m add_rem_watches
    return ()
    where
      uuid = vmm_uuid m
      add_rem_watches _ (VmStateChange Created ) = addWatches uuid watches
      add_rem_watches _ (VmStateChange Shutdown) = remWatches watches
      add_rem_watches _ (VmStateChange Rebooted) = remWatches watches
      add_rem_watches _ _ = return ()

evloop :: VmMonitor -> Rpc ()
evloop m = mapM_ process =<< liftIO (vmm_events m) where
    process e = evalVmEvent m e

data VmWatch = VmWatch { watch_path   :: String
                       , watch_action :: IO ()
                       , watch_active :: MVar Bool
                       , watch_quit   :: MVar Bool
                       , watch_thread :: MVar ThreadId }

vmStateWatch :: VmMonitor -> Rpc ()
vmStateWatch m = let uuid = vmm_uuid m in do
    watch <- liftIO $ stateWatch (vmm_submit m)
    addWatch uuid watch
    return ()

newVmWatch :: String -> IO () -> IO VmWatch
newVmWatch path f =
    newMVar False >>= \quit ->
    newMVar False >>= \active ->
    newEmptyMVar >>= \thread ->
    return $ VmWatch path f active quit thread

killVmWatch :: VmWatch -> IO ()
killVmWatch (VmWatch _ _ active quit threadID) =
    do active' <- takeMVar active
       when active' $ do
         modifyMVar_ quit $ \_ -> return True
         id <- takeMVar threadID
         killThread id
       putMVar active False

addWatch :: Uuid ->VmWatch -> Rpc ()
addWatch uuid ws =
    do
        liftIO $ mapM_ (add ("/state/" ++ show uuid)) [ws]
    where
      add vm_path (VmWatch path pred active quit thread_id) =
          forkIO $ do
            myThreadId >>= \id -> putMVar thread_id id
            modifyMVar_ active (\_ -> return True)
            modifyMVar_ quit   (\_ -> return False)
            xsWaitFor (vm_path++path) (pred >> readMVar quit)

addWatches :: Uuid -> [VmWatch] -> Rpc ()
addWatches uuid ws =
    do remWatches ws
       whenDomainID_ uuid $ \domid ->
           do let vm_path = "/local/domain/" ++ show domid
              liftIO $ mapM_ (add vm_path) ws
    where
      add vm_path (VmWatch path pred active quit thread_id) =
          forkIO $ do
            myThreadId >>= \id -> putMVar thread_id id
            modifyMVar_ active (\_ -> return True)
            modifyMVar_ quit   (\_ -> return False)
            xsWaitFor (vm_path++path) (pred >> readMVar quit)

remWatches :: [VmWatch] -> Rpc ()
remWatches ws = liftIO $ mapM_ killVmWatch ws

stateWatch :: (VmEvent -> IO ()) -> IO VmWatch
stateWatch submit = newVmWatch "/state" (submit VmStateUpdate)

vmStateSubmit :: VmMonitor -> IO ()
vmStateSubmit m = (vmm_submit m) VmStateUpdate

watchesForVm :: (VmEvent -> IO ()) -> [IO VmWatch]
watchesForVm submit =
    [ newVmWatch "/attr/PVAddons" (submit VmPvAddonsNodeChange)
    , newVmWatch "/attr/PVAddonsUninstalled" (submit VmPvAddonsUninstallNodeChange)
    , newVmWatch "/bsgdev" (submit VmBsgDevNodeChange)
    , newVmWatch "/acpi-state" (submit VmAcpiUpdate)
    ]

