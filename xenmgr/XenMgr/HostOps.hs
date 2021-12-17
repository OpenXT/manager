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

module XenMgr.HostOps ( hostShutdown
                      , hostShutdownIdle
                      , hostSleep
                      , hostHibernate
                      , hostReboot
                      , hostChangeAutolockCdDrives
                      ) where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import Control.Concurrent
import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Applicative

import Tools.Log
import Tools.Misc
import XenMgr.Rpc
import Vm.Types
import Vm.Queries
import Vm.Actions
import XenMgr.Host
import XenMgr.PowerManagement
import XenMgr.XM
import XenMgr.Config

-- These operations fail silently if host is not in idle state, i.e. is doing another action already
hostShutdown :: XM ()
hostShutdown = (hostWhenIdleDoWithState HostShuttingDown $ executePmAction ActionShutdown) >> return ()

hostShutdownIdle :: XM ()
hostShutdownIdle = executePmAction ActionIdleShutdown >> return ()

hostSleep :: XM ()
hostSleep = (hostWhenIdleDoWithState HostGoingToSleep $ executePmAction ActionSleep) >> return ()

hostHibernate :: XM ()
hostHibernate = (hostWhenIdleDoWithState HostGoingToHibernate $ executePmAction ActionHibernate) >> return ()

hostReboot :: XM ()
hostReboot = (hostWhenIdleDoWithState HostRebooting $ executePmAction ActionReboot) >> return ()

hostChangeAutolockCdDrives :: Bool -> Rpc ()
hostChangeAutolockCdDrives v = do
  appSetAutolockCdDrives v
  mapM_ update =<< (filterM isRunning =<< getVms)
  where
    update uuid = setVmAutolockCdDrives uuid v
