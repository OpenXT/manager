--
-- Copyright (c) 2012 Citrix Systems, Inc.
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
module AppInventory
       (
         AppState (..)
       , enumAppliances
       , findAppliance
       , findVmAppliances
       , applianceOwnedFiles
       , applianceOwnedVms
       , haveAppliance
       , registerAppliance
       , registerApplianceOwnedFile
       , registerApplianceOwnedVm
       , registerApplianceVmDisk
       , unregisterAppliance
       , getSystemID
       , appDBPath
       , changeApplianceState
       ) where


import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Data.String

import Tools.Db
import Tools.IfM
import Tools.Text
import Tools.File

import System.Directory

import Rpc.Core
import Rpc.Autogen.XenmgrVmClient
import Rpc.Autogen.XenmgrClient

import Core.Types
import Appliance
import VirtualSystem
import Util

data AppState =
    AppInstalling
  | AppUninstalling
  | AppInstalled
    deriving (Eq, Show)

instance EnumMarshall AppState where
    enumMarshallMap = [ (AppInstalling , "installing" )
                      , (AppUninstalling, "uninstalling" )
                      , (AppInstalled, "installed") ]

instance Marshall AppState where {dbRead = dbReadEnum; dbWrite = dbWriteEnum}

xenmgrObj  = "/"
xenmgrSrv  = "com.citrix.xenclient.xenmgr"

withXenmgr f = f xenmgrSrv xenmgrObj
withXenmgrVmUuid uuid  f = f xenmgrSrv vmpath where
  vmpath = "/vm/" ++ uuidStrUnderscore uuid

appDBPath (AppID name _) = "/appliance/" ++ name
appFilesDBPath app = appDBPath app ++ "/file"
appStateDBPath app = appDBPath app ++ "/state"
appVmsDBPath app = appDBPath app ++ "/vm"
appSystemIDDBPath app vmuuid = appVmsDBPath app ++ "/" ++ show vmuuid ++ "/id"

enumAppliances :: MonadRpc e m => m [AppID]
enumAppliances = mapM makeAppID =<< dbList "/appliance" where
  makeAppID name =
    AppID name <$> (read <$> dbReadWithDefault "1" ("/appliance/"++name++"/version"))

applianceOwnedFiles :: MonadRpc e m => AppID -> m [FilePath]
applianceOwnedFiles app = dbRead (appFilesDBPath app)

applianceOwnedVms :: MonadRpc e  m => AppID -> m [Uuid]
applianceOwnedVms app = map fromString <$> dbList (appVmsDBPath app)

haveAppliance :: MonadRpc e m => AppID -> m Bool
haveAppliance id = dbExists (appDBPath id)

registerAppliance :: MonadRpc e m => AppID -> m ()
registerAppliance id@(AppID _ version) = do
  whenM (haveAppliance id) $ error ("appliance " ++ show id ++ " already registered")
  dbWrite (appDBPath id ++ "/version") version
  changeApplianceState id AppInstalling

registerApplianceOwnedFile :: MonadRpc e m => AppID -> FilePath -> m ()
registerApplianceOwnedFile app p =
  dbWrite (appFilesDBPath app) =<< ((++ [p]) <$> applianceOwnedFiles app)
  
registerApplianceOwnedVm :: MonadRpc e m => AppID -> Uuid -> VirtualSystemID -> m ()
registerApplianceOwnedVm app uuid (VirtualSystemID sysid) = do 
  dbWrite (appSystemIDDBPath app uuid) sysid
  
registerApplianceVmDisk :: MonadRpc e m => AppID -> Disk -> String -> m ()
registerApplianceVmDisk app disk diskObj
  | Just im <- diskImage disk = dbWrite (appDBPath app ++ "/disk/" ++ sysidstr ++ "/" ++ imageid im) diskObj
  | otherwise = return ()
  where
    VirtualSystemID sysidstr = sysid
    DiskID sysid index = diskID disk
    imageid im = let DiskImageID id = diID im in id
    
-- FIXME: app db locking
unregisterAppliance :: MonadRpc e m => AppID -> Bool -> Bool -> m ()
unregisterAppliance app removeVms removeAssets =
  whenM (haveAppliance app) $ do
    changeApplianceState app AppUninstalling
    vms <- applianceOwnedVms app
    running <- runningVms vms
    when removeVms    $  when (not . null $ running) $ error ("cannot remove appliance because vms are running: " ++ show running)
    when removeVms    $  mapM_ removeAppVm vms
    when removeAssets $ do
      -- only remove files not reused by other appliances
      other_apps <- filter (/= app) <$> enumAppliances
      other_app_files <- concat <$> mapM applianceOwnedFiles other_apps
      mapM_ (removeAppFile other_app_files) =<< applianceOwnedFiles app
      -- liftIO $ rmDirRec (appStoragePath app)
    dbRm (appDBPath app)
  where
    runningVms vms = filterM (\uuid -> (/= eVM_STATE_STOPPED) <$> withXenmgrVmUuid uuid comCitrixXenclientXenmgrVmGetState) vms
    removeAppVm uuid = withXenmgrVmUuid uuid comCitrixXenclientXenmgrVmDelete
    removeAppFile exclude f 
      | not (f `elem` exclude) = liftIO $ whenM (doesFileExist f) (removeFile f)
      | otherwise = return ()

changeApplianceState :: MonadRpc e m => AppID -> AppState -> m ()
changeApplianceState app = dbWrite (appStateDBPath app)

findAppliance :: MonadRpc e m => String -> m (Maybe AppID)
findAppliance id = unlist . filter matches <$> enumAppliances where
  matches (AppID id' _) = id == id'
  unlist (app:_) = Just app
  unlist _ = Nothing

findVmAppliances :: MonadRpc e m => Uuid -> m [AppID]
findVmAppliances vm_uuid = filterM matches =<< enumAppliances where
  matches app = (vm_uuid `elem`) <$> applianceOwnedVms app
    
getSystemID :: MonadRpc e m => AppID -> Uuid -> m VirtualSystemID
getSystemID app vm =
  VirtualSystemID <$> dbRead (appSystemIDDBPath app vm)
  
