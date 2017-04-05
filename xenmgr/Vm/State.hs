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

module Vm.State where

import Control.Applicative
import Data.Maybe
import XenMgr.Rpc
import Vm.Types
import Tools.XenStore
import Rpc.Autogen.XenmgrConst
import System.Timeout
import Text.Printf
import Tools.Log

stateFromStr :: String -> VmState
stateFromStr "pre-create"  = PreCreate
stateFromStr "creating-domain"  = CreatingDomain
stateFromStr "creating-devices" = CreatingDevices
stateFromStr "created"     = Created
stateFromStr "running"     = Running
stateFromStr "shutdown"    = Shutdown
stateFromStr "shutdowning" = ShuttingDown
stateFromStr "rebooting"   = Rebooting
stateFromStr "rebooted"    = Rebooted
stateFromStr "suspended"   = Suspended
stateFromStr "suspending"  = Suspending
stateFromStr "restoring"   = Restoring
stateFromStr "paused"      = Paused
stateFromStr s             = error $ "unexpected VM state string: " ++ s

stateToStr :: VmState -> String
stateToStr PreCreate = "pre-create"
stateToStr CreatingDomain = "creating-domain"
stateToStr CreatingDevices = "creating-devices"
stateToStr Created = "created"
stateToStr Running = "running"
stateToStr Shutdown =  "shutdown"
stateToStr ShuttingDown = "shutdowning"
stateToStr Rebooting = "rebooting"
stateToStr Rebooted = "rebooted"
stateToStr Suspended = "suspended"
stateToStr Suspending = "suspending"
stateToStr Restoring = "restoring"
stateToStr Paused = "paused"

stateToPublicStr :: VmState -> String
stateToPublicStr PreCreate       = eVM_STATE_CREATING
stateToPublicStr CreatingDomain  = eVM_STATE_CREATING
stateToPublicStr CreatingDevices = eVM_STATE_CREATING
stateToPublicStr Created         = eVM_STATE_CREATING
stateToPublicStr Running         = eVM_STATE_RUNNING
stateToPublicStr ShuttingDown    = eVM_STATE_STOPPING
stateToPublicStr Rebooting       = eVM_STATE_REBOOTING
stateToPublicStr Rebooted        = eVM_STATE_REBOOTED
stateToPublicStr Suspending      = eVM_STATE_SUSPENDING
stateToPublicStr Suspended       = eVM_STATE_SUSPENDED
stateToPublicStr Restoring       = eVM_STATE_RESTORING
stateToPublicStr Paused          = eVM_STATE_PAUSED
stateToPublicStr Shutdown        = eVM_STATE_STOPPED

internalStatePath :: Uuid -> String
internalStatePath uuid = "/vm/" ++ show uuid ++ "/state"

updateVmInternalState :: MonadRpc e m => Uuid -> VmState -> m ()
updateVmInternalState uuid s = liftIO (xsWrite (internalStatePath uuid) (stateToStr s))

getVmInternalState :: MonadRpc e m => Uuid -> m VmState
getVmInternalState uuid = fromMaybe Shutdown . fmap stateFromStr <$> (liftIO $ xsRead (internalStatePath uuid))

waitForVmInternalState :: MonadRpc e m => Uuid -> VmState -> VmState -> Int -> m ()
waitForVmInternalState uuid state cond_state to_secs = do
  i_state <- getVmInternalState uuid
  info $ "waitForState: i_state: " ++ (stateToStr i_state) ++ " state: " ++ (stateToStr state)
  if (stateToStr i_state) == (stateToStr state)  || ((stateToStr i_state) == (stateToStr cond_state)) then do return ()
    else do
           info $ printf "Wait for vm %s state to become %s" (show uuid) stateStr
           handle =<< ( liftIO $ timeout (10^6 * to_secs) (xsWaitFor (internalStatePath uuid) check) )
  where
    stateStr = stateToStr state
    check = (== Just stateStr) <$> xsRead (internalStatePath uuid)
    handle Nothing = error $ "Timeout while waiting for vm " ++ show uuid ++ " state to become " ++ stateStr ++ " after " ++ show to_secs ++ "s"
    handle _  = return ()

  
