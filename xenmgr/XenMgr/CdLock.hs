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

{-# LANGUAGE ScopedTypeVariables #-}

module XenMgr.CdLock
       (
         getCdDeviceVms
       , getCdDeviceStickyVm
       , getVmStickyCdDevices
       , assignCdDevice
       , assignStickyCdDevice
       , unassignCdDevice
       , unassignStickyCdDevice
       , ejectCdDevice
       , notifyCdDeviceAssignmentChanged
       , updateCdDeviceMediaStatusKey
       ) where

import Data.Maybe
import Data.String
import Control.Applicative
import Control.Monad
import qualified Control.Exception as E
import Text.Printf
import System.Timeout
import System.IO.Unsafe
import System.FilePath
import Vm.Types
import Vm.DomainCore
import Vm.Queries
import XenMgr.Rpc
import XenMgr.Host
import Tools.XenStore
import Tools.Db
import Tools.Log
import Tools.Misc
import Tools.File
import Tools.Process
import System.Directory
import Control.Concurrent
import Rpc.Autogen.XenmgrNotify
import XenMgr.Notify
import XenMgr.Expose.ObjectPaths
import Data.Map (Map)
import qualified Data.Map as Map

requestTimeout = 5

-- cd assignment
xsWaitForNodeToDisappear :: Int -> String -> IO Bool
xsWaitForNodeToDisappear timeout_secs node = do
    r <- timeout (10^6 * timeout_secs) $ xsWaitFor node test
    case r of
      Just () -> return True
      Nothing -> return False
  where
    test = isNothing <$> xsRead node

cdDeviceXsNode :: DomainID -> BSGDevice -> String
cdDeviceXsNode domid (BSGDevice a b c d) =
  printf "/local/domain/%d/bsgdev/%s" domid (printf "%d_%d_%d_%d" a b c d :: String)

cdDeviceXsReqNode :: DomainID -> BSGDevice -> String
cdDeviceXsReqNode domid (BSGDevice a b c d) =
  printf "/local/domain/%d/bsgdev-req/%s" domid (printf "%d_%d_%d_%d" a b c d :: String)

cdDeviceStickyNode :: BSGDevice -> String
cdDeviceStickyNode (BSGDevice a b c d) =
    printf "/xenmgr/cdassign/%s" (printf "%d_%d_%d_%d" a b c d :: String)

getCdDeviceLockStatus :: DomainID -> BSGDevice -> IO Bool
getCdDeviceLockStatus domid dev = from <$> xsRead (cdDeviceXsNode domid dev ++ "/lock") where
  from (Just "1") = True
  from _ = False

getCdDeviceMediaStatus :: BSGDevice -> IO Bool
getCdDeviceMediaStatus dev =
  fromBdev =<< findBlockDevice dev where
    fromBdev Nothing = return False
    fromBdev (Just bdev) =
      (parse <$> readProcessOrDie "/lib/udev/cdrom_id" [bdev] "")
        `E.catch` (\(_ :: E.SomeException) -> return False)
    parse = ("ID_CDROM_MEDIA=1" `elem`) . lines

updateCdDeviceMediaStatusKey :: BSGDevice -> IO ()
updateCdDeviceMediaStatusKey dev =
  update (key dev) =<< (strstate <$> getCdDeviceMediaStatus dev) where
    key (BSGDevice a b c d) = printf "/xenclient/bsgdev/%d_%d_%d_%d/media" a b c d
    strstate True = "1"
    strstate _ = "0"
    update path v = do
      v' <- xsRead path
      when (Just v /= v') $ xsWrite path v

getCdDeviceDomains :: BSGDevice -> Rpc [DomainID]
getCdDeviceDomains dev = filterM lockedBy =<< getDomains where
  getDomains = catMaybes <$> (mapM getDomainID =<< getVms)
  lockedBy domid = liftIO $ getCdDeviceLockStatus domid dev

-- return (vm uuid, sticky bit) tuples
getCdDeviceVms :: BSGDevice -> Rpc [(Uuid,Bool)]
getCdDeviceVms dev = do
  running  <- catMaybes <$> (mapM getDomainUuid =<< getCdDeviceDomains dev)
  stickies <- getVmsBy (isStickyTo dev)
  return $ map (\vm -> (vm, vm `elem` stickies)) (running ++ stickies)
  
unassignCdDevice' :: BSGDevice -> DomainID -> IO ()
unassignCdDevice' dev domid = do
  assigned <- getCdDeviceLockStatus domid dev
  when assigned $ do
    let ejectnode = cdDeviceXsReqNode domid dev ++ "/req-eject"
    xsWrite ejectnode "1"
    r <- xsWaitForNodeToDisappear requestTimeout ejectnode
    when (not r) $ error "unassign failed"
    
unassignCdDevice :: BSGDevice -> Rpc ()
unassignCdDevice dev = mapM_ (liftIO . unassignCdDevice' dev) =<< getCdDeviceDomains dev

assignCdDevice :: BSGDevice -> Uuid -> Rpc ()
assignCdDevice dev@(BSGDevice a b c d) uuid = withDomain =<< getDomainID uuid where
  withDomain Nothing = return ()
  withDomain (Just domid) = liftIO $ do
    assigned <- getCdDeviceLockStatus domid dev
    when (not assigned) $ do
      info $ "assigning CD device " ++ (printf "%d:%d:%d:%d" a b c d) ++ " to vm " ++ show uuid
      let assignnode = cdDeviceXsReqNode domid dev ++ "/req-assign"
      xsWrite assignnode "1"
      r <- xsWaitForNodeToDisappear requestTimeout assignnode
      when (not r) $ error "assign of CD device failed"

unassignStickyCdDevice :: BSGDevice -> Rpc ()
unassignStickyCdDevice dev = do
  dbRm (cdDeviceStickyNode dev)
  unassignCdDevice dev

assignStickyCdDevice :: BSGDevice -> Uuid -> Rpc ()
assignStickyCdDevice dev uuid = do
  unassignCdDevice dev
  dbWrite (cdDeviceStickyNode dev) uuid
  withDomain =<< getDomainID uuid
  where
    withDomain (Just domid) = assignCdDevice dev uuid
    withDomain _ = return ()

getCdDeviceStickyVm :: BSGDevice -> Rpc (Maybe Uuid)
getCdDeviceStickyVm dev = do
  vm' <- dbRead (cdDeviceStickyNode dev)
  case vm' of
    "" -> return Nothing
    uuidStr -> return $ Just (fromString uuidStr)

isStickyTo :: BSGDevice -> Uuid -> Rpc Bool
isStickyTo dev vm = (== Just vm) <$> getCdDeviceStickyVm dev

getVmStickyCdDevices :: Uuid -> Rpc [BSGDevice]
getVmStickyCdDevices uuid = filterM (\d -> isStickyTo d uuid) =<< liftIO getHostBSGDevices

ejectCdDevice :: BSGDevice -> IO ()
ejectCdDevice dev = ej =<< findBlockDevice dev where
  ej Nothing = return ()
  ej (Just block) = do
    info $ "ejecting " ++ block
    void $ readProcessOrDie "eject" [block] []

findBlockDevice :: BSGDevice -> IO (Maybe FilePath)
findBlockDevice scsi =
  get <$> (filterM test =<< list "/sys/class/block") where
    list path = map (path </>) <$> getDirectoryContents_nonDotted path
    test path = doesDirectoryExist (path </> "device" </> "scsi_device" </> fname scsi)
    fname (BSGDevice a b c d) = printf "%d:%d:%d:%d" a b c d
    get [] = Nothing
    get (x:_) = Just ("/dev" </> takeBaseName x)
  
cdAssignChangedTasks :: MVar (Map BSGDevice ScheduledTask)
{-# NOINLINE cdAssignChangedTasks #-}
cdAssignChangedTasks = unsafePerformIO (newMVar Map.empty)

notifyCdDeviceAssignmentChanged :: (MonadRpc e m) => BSGDevice -> m ()
notifyCdDeviceAssignmentChanged dev = do
  updateKeyedNotifyTask 1.0 dev cdAssignChangedTasks $ do
    action =<< getCdDeviceVms dev
  where
    action [] =
      notifyComCitrixXenclientXenmgrCdAssignmentChanged xenmgrObjectPath (bsgDeviceIdStr dev) "" (fromString "/")
    action ((vm,sticky):_) = do
      -- update media state on lock transference
      liftIO $ updateCdDeviceMediaStatusKey dev
      -- dbus signal
      notifyComCitrixXenclientXenmgrCdAssignmentChanged xenmgrObjectPath (bsgDeviceIdStr dev) (show vm) (vmObjPath vm)
