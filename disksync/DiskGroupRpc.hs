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

module DiskGroupRpc ( expose )
    where

import Data.String
import Data.Maybe
import Control.Applicative
import Control.Monad ( liftM )

import Tools.Misc
import Tools.Log
import Config

import Rpc
import Rpc.Autogen.DisksyncmgrDiskgroupServer
import Db
import Types
import ObjectPaths
import qualified DiskRpc
import qualified ImageRpc
import DiskGroup
import Disk
import DiskSync

expose :: AppContext -> GroupId -> Rpc ()
expose ctx group_id =
    do rpcExpose path (interfaces $ implementation ctx group_id)
       setEnableDvdCache True
       setEnableUsbCache True
       info $ "exposed group " ++ show group_id ++ " over DBUS at " ++ show path
    where path = diskGroupObjPath group_id

unexpose :: GroupId -> Rpc ()
unexpose group_id =
    do rpcHide path
       info $ "removed group " ++ show group_id ++ " from DBUS at " ++ show path
    where path = diskGroupObjPath group_id

implementation ctx group_id =
    DisksyncmgrDiskgroupServer
    {
      comCitrixXenclientDisksyncmgrDiskgroupListDisks =
          disksync $ groupGetDisks group_id >>= return . map diskObjPath
    , comCitrixXenclientDisksyncmgrDiskgroupCreateDisk =
        \uuid_str dir_str ->
            do let disk_uuid = fromString uuid_str
                   dir = dirOfString dir_str
               disksync $ do
                 disk_id <- diskCreate group_id disk_uuid dir
                 liftRpc $ DiskRpc.expose ctx disk_id
                 return (diskObjPath disk_id)
    , comCitrixXenclientDisksyncmgrDiskgroupStart = disksync $ groupStart group_id
    , comCitrixXenclientDisksyncmgrDiskgroupStop = disksync $ groupStopAndStartNext group_id
    , comCitrixXenclientDisksyncmgrDiskgroupPause = disksync $ groupPause group_id
    , comCitrixXenclientDisksyncmgrDiskgroupResume = disksync $ groupResume group_id
    , comCitrixXenclientDisksyncmgrDiskgroupDelete = disksync $
        do let flatten = foldl (++) []
           disks <- groupGetDisks group_id
           -- a-list disk_uuid => [img_uuid]
           images <- flatten `liftM` mapM diskGetImages disks
           groupDelete group_id
           liftRpc $ do mapM ImageRpc.unexpose images
                        mapM DiskRpc.unexpose disks
                        unexpose group_id

    , comCitrixXenclientDisksyncmgrDiskgroupGetId = return (show group_id)
    , comCitrixXenclientDisksyncmgrDiskgroupGetState =
        disksync $ return . groupStateStr =<< groupGetState group_id
    , comCitrixXenclientDisksyncmgrDiskgroupGetStage =
        disksync $ return . with_default stage "" =<< groupGetProgress group_id
    , comCitrixXenclientDisksyncmgrDiskgroupGetPercentComplete =
        disksync $ return . with_default completeness 0.0 =<< groupGetProgress group_id
    , comCitrixXenclientDisksyncmgrDiskgroupGetUploadSpeed =
        disksync $ return . with_default ul 0.0 =<< groupGetProgress group_id
    , comCitrixXenclientDisksyncmgrDiskgroupGetDnloadSpeed =
        disksync $ return . with_default dl 0.0 =<< groupGetProgress group_id
    , comCitrixXenclientDisksyncmgrDiskgroupGetAdmctlQpos =
        disksync $ return . fromIntegral <$> with_default qpos 0 =<< groupGetAdmctl group_id
    , comCitrixXenclientDisksyncmgrDiskgroupGetAdmctlRetryTime =
        disksync $ return . with_default retry 0.0 =<< groupGetAdmctl group_id
    }
    where
      disksync f = runDiskSync ctx f

      with_default f d x | x == Nothing = d
                         | otherwise    = f $ fromJust x

      stage        (x,_,_,_) = x
      completeness (_,x,_,_) = x
      ul           (_,_,x,_) = x
      dl           (_,_,_,x) = x

      qpos  (x,_) = x
      retry (_,x) = x

