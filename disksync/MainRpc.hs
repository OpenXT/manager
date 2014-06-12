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

module MainRpc ( expose )
    where

import Data.String
import Data.List
import Tools.Misc
import Tools.Log
import Control.Monad
import Control.Applicative

import Rpc
import Rpc.Autogen.DisksyncmgrServer
import Db
import Types
import ObjectPaths
import DiskGroup
import Disk
import Image
import Config
import qualified DiskGroupRpc
import DiskSync
import qualified Data.Text.Lazy as TL

expose :: AppContext -> Rpc ()
expose ctx =
    do rpcExpose path (interfaces $ implementation ctx)
       info $ "exposed primary dbus object"
    where path = fromString "/"

dumpGroup :: GroupId -> DiskSync String
dumpGroup group_id =
    do maybe_group <- liftRpc (dbMaybeRead $ diskGroupDBPath group_id) :: DiskSync (Maybe GroupId)
       dump maybe_group
    where
    dump Nothing  = return "?"
    dump (Just x) = do
      state <- groupGetState group_id
      return $
             TL.unpack (strObjectPath (diskGroupObjPath group_id)) ++ " " ++
             show state

dumpDisk :: DiskId -> DiskSync String
dumpDisk disk_id = liftRpc (dbMaybeRead $ diskDBPath disk_id) >>= dump where
    dump Nothing  = return "?"
    dump (Just x) = do
      group_id <- diskGetGroup x
      state <- diskGetState disk_id
      return $
             TL.unpack (strObjectPath (diskObjPath disk_id)) ++ " " ++
             TL.unpack (strObjectPath (diskGroupObjPath group_id)) ++ " " ++
             show state

dumpImage :: ImageId -> DiskSync String
dumpImage image_id = liftRpc (dbMaybeRead $ imageDBPath image_id) >>= dump where
    dump Nothing  = return "?"
    dump (Just x) = do
      disk_id <- imageGetDisk x
      state <- imageGetState image_id
      return $
             TL.unpack (strObjectPath (imageObjPath image_id)) ++ " " ++
             TL.unpack (strObjectPath (diskObjPath disk_id)) ++ " " ++
             show state

implementation ctx =
    DisksyncmgrServer
    {
      comCitrixXenclientDisksyncmgrListDiskGroups =
          disksync $ groupList >>= return . map diskGroupObjPath
    , comCitrixXenclientDisksyncmgrCreateDiskGroup =
        disksync $ do
          group_id <- groupCreate
          liftRpc $ DiskGroupRpc.expose ctx group_id
          return (diskGroupObjPath group_id)
    , comCitrixXenclientDisksyncmgrDebugDumpTasks =
        disksync $ do
          groups <- mapM dumpGroup =<< groupList
          disks <- mapM dumpDisk =<< diskList
          images <- mapM dumpImage =<< imageList
          return . concat $ intersperse "\n" (groups ++ disks ++ images)
    , comCitrixXenclientDisksyncmgrConfigGetEnableDvdCache = getEnableDvdCache
    , comCitrixXenclientDisksyncmgrConfigSetEnableDvdCache = setEnableDvdCache
    , comCitrixXenclientDisksyncmgrConfigGetEnableUsbCache = getEnableUsbCache
    , comCitrixXenclientDisksyncmgrConfigSetEnableUsbCache = setEnableUsbCache
    , comCitrixXenclientDisksyncmgrConfigSetForceLocalChecksum = setEnableForceLocalCheckSum
    , comCitrixXenclientDisksyncmgrConfigGetForceLocalChecksum = getEnableForceLocalCheckSum
    , comCitrixXenclientDisksyncmgrConfigGetEnableUploadCompaction = getEnableUploadCompaction
    , comCitrixXenclientDisksyncmgrConfigSetEnableUploadCompaction = setEnableUploadCompaction
    , comCitrixXenclientDisksyncmgrConfigGetDownloadCksize = fromIntegral <$> getDownloadCksize
    , comCitrixXenclientDisksyncmgrConfigSetDownloadCksize = setDownloadCksize . fromIntegral
    , comCitrixXenclientDisksyncmgrConfigGetUploadCksize = fromIntegral <$> getUploadCksize
    , comCitrixXenclientDisksyncmgrConfigSetUploadCksize = setUploadCksize . fromIntegral
    , comCitrixXenclientDisksyncmgrConfigGetVhdsyncLogLevel = fromIntegral <$> getVhdsyncLogLevel
    , comCitrixXenclientDisksyncmgrConfigSetVhdsyncLogLevel = setVhdsyncLogLevel . fromIntegral
    , comCitrixXenclientDisksyncmgrConfigGetVhdsyncVerbose = getVhdsyncVerbose
    , comCitrixXenclientDisksyncmgrConfigSetVhdsyncVerbose = setVhdsyncVerbose
    , comCitrixXenclientDisksyncmgrConfigGetXferConnectTimeout = fromIntegral <$> getXferConnectTimeout
    , comCitrixXenclientDisksyncmgrConfigSetXferConnectTimeout = setXferConnectTimeout . fromIntegral
    , comCitrixXenclientDisksyncmgrConfigGetXferLowspeedLimit = fromIntegral <$> getXferLowspeedLimit
    , comCitrixXenclientDisksyncmgrConfigSetXferLowspeedLimit = setXferLowspeedLimit . fromIntegral
    , comCitrixXenclientDisksyncmgrConfigGetXferLowspeedTime = fromIntegral <$> getXferLowspeedTime
    , comCitrixXenclientDisksyncmgrConfigSetXferLowspeedTime = setXferLowspeedTime . fromIntegral
    }
    where
      disksync f = runDiskSync ctx f
