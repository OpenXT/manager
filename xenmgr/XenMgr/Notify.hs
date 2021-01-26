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

module XenMgr.Notify (
                       notifyHostStateChanged
                     , notifyLicenseChanged
                     , notifyVmConfigChanged
                     , notifyVmNameChanged
                     , notifyVmCreated
                     , notifyVmDeleted
                     , notifyVmTransferChanged
                     , notifyStorageSpaceLow
                     , notifyDiagGatherRequest
                     , updateKeyedNotifyTask
                     , notifyDisplayHandlerGpuChanged
                     ) where
import Data.String
import Data.Int
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent
import System.IO.Unsafe
import XenMgr.Rpc
import Rpc.Autogen.XenmgrNotify
import Rpc.Autogen.XenmgrHostNotify
import XenMgr.Expose.ObjectPaths
import Vm.Types
import Tools.Misc
import Data.Time

notifyHostStateChanged :: (MonadRpc e m) => String -> m ()
notifyHostStateChanged state =
    notifyComCitrixXenclientXenmgrHostStateChanged hostObjectPath state

notifyLicenseChanged :: (MonadRpc e m) => m ()
notifyLicenseChanged =
  notifyComCitrixXenclientXenmgrHostLicenseChanged (fromString "/host")

notifyDisplayHandlerGpuChanged :: (MonadRpc e m) => m ()
notifyDisplayHandlerGpuChanged =
  notifyComCitrixXenclientXenmgrHostDisplayhandlerGpuChanged (fromString "/host")

configChangedTasks :: MVar (Map Uuid ScheduledTask)
{-# NOINLINE configChangedTasks #-}
configChangedTasks = unsafePerformIO (newMVar Map.empty)

-- creates new notification task or reschedules currently pending task into future. Used to
-- throttle spammy notifications
updateKeyedNotifyTask :: (Ord k, MonadRpc e m) => DiffTime -> k -> MVar (Map k ScheduledTask) -> Rpc () -> m ()
updateKeyedNotifyTask throttle key amap action = do
  ctx <- rpcGetContext
  liftIO $ modifyMVar_ amap $ \m -> upd m (Map.lookup key m) (act ctx)
  where
    upd m Nothing act
      = do t <- schedule throttle act
           return $ Map.insert key t m
    upd m (Just t) act
      = do reschedule throttle t
           return $ m
    act ctx
      = do modifyMVar_ amap $ \m -> return $ Map.delete key m
           void $ rpc ctx action

notifyVmConfigChanged :: (MonadRpc e m) => Uuid -> m ()
notifyVmConfigChanged uuid = do
  updateKeyedNotifyTask 0.5 uuid configChangedTasks $
      notifyComCitrixXenclientXenmgrVmConfigChanged xenmgrObjectPath (show uuid) (vmObjPath uuid)

notifyVmNameChanged :: (MonadRpc e m) => Uuid -> m ()
notifyVmNameChanged uuid =
      notifyComCitrixXenclientXenmgrVmNameChanged xenmgrObjectPath (show uuid) (vmObjPath uuid)

notifyVmCreated :: (MonadRpc e m) => Uuid -> m ()
notifyVmCreated uuid =
    notifyComCitrixXenclientXenmgrVmCreated xenmgrObjectPath (show uuid) (vmObjPath uuid)

notifyVmDeleted :: (MonadRpc e m) => Uuid -> m ()
notifyVmDeleted uuid =
    notifyComCitrixXenclientXenmgrVmDeleted xenmgrObjectPath (show uuid) (vmObjPath uuid)

notifyVmTransferChanged :: (MonadRpc e m) => Uuid -> m ()
notifyVmTransferChanged uuid =
    notifyComCitrixXenclientXenmgrVmTransferChanged xenmgrObjectPath (show uuid) (vmObjPath uuid)

notifyStorageSpaceLow :: (MonadRpc e m) => Int -> m ()
notifyStorageSpaceLow percentFree =
    let pf = fromIntegral percentFree :: Int32 in
    notifyComCitrixXenclientXenmgrHostStorageSpaceLow hostObjectPath pf

notifyDiagGatherRequest :: (MonadRpc e m) => String -> m ()
notifyDiagGatherRequest mode =
    notifyComCitrixXenclientXenmgrDiagGatherRequest xenmgrObjectPath mode
