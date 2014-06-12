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

module DiskRpc ( expose, unexpose )
    where

import Maybe ( fromJust, isJust )
import Data.String

import Tools.Misc
import Tools.Log

import Rpc
import Rpc.Autogen.DisksyncmgrDiskServer
import qualified ImageRpc
import Db
import Disk
import Image
import Types
import DiskSync
import ObjectPaths

expose :: AppContext -> DiskId -> Rpc ()
expose ctx disk_id =
    do rpcExpose path (interfaces $ implementationFor ctx disk_id)
       info $ "exposed disk " ++ show disk_id ++ " over DBUS at " ++ show path
    where
      path = diskObjPath disk_id

unexpose :: DiskId -> Rpc ()
unexpose disk_id =
    do rpcHide path
       info $ "removed disk " ++ show disk_id ++ " from DBUS at " ++ show path
    where
      path = diskObjPath disk_id

maybeImageFailureReason (ImageFailed x) = Just x
maybeImageFailureReason _               = Nothing

implementationFor ctx disk_id = self where
  self =
      DisksyncmgrDiskServer
      {
        comCitrixXenclientDisksyncmgrDiskGetId = return $ show disk_id
      , comCitrixXenclientDisksyncmgrDiskGetGroupId = get_str "group-id"
      , comCitrixXenclientDisksyncmgrDiskGetDiskUuid = get_str "disk-uuid"
      , comCitrixXenclientDisksyncmgrDiskGetDirection = get_str "direction"
      , comCitrixXenclientDisksyncmgrDiskGetState =
          disksync $ return . diskStateStr =<< diskGetState disk_id
      , comCitrixXenclientDisksyncmgrDiskGetFailureReason =
          disksync $ do
            states <- mapM imageGetState =<< diskGetImages disk_id
            let maybe_reasons = map maybeImageFailureReason states
            let reasons = map fromJust . filter isJust $ maybe_reasons
            return $ case reasons of
                       [] -> ""
                       x:_ -> stringOfReason x
      , comCitrixXenclientDisksyncmgrDiskListImages =
          disksync $ diskGetImages disk_id >>= return . map imageObjPath
      , comCitrixXenclientDisksyncmgrDiskCreateImage =
          \uuid_str im_url xfer_url sv_cert cl_cert cl_key encrypt_local ctxid ->
              do let image_uuid = fromString uuid_str
                 disksync $ do
                   image_ordinal <- next_image_ordinal
                   image_id <- imageCreate disk_id image_ordinal image_uuid im_url xfer_url sv_cert cl_cert cl_key encrypt_local ctxid
                   liftRpc $ ImageRpc.expose ctx image_id
                   return $ imageObjPath image_id
      }
      where
        disksync f = runDiskSync ctx f

        get_str :: String -> Rpc String
        get_str prop = dbReadWithDefault ( diskDBPath disk_id ++ "/" ++ prop ) ""

        next_image_ordinal =
            do ordinals <- mapM imageGetOrdinal =<< diskGetImages disk_id
               return $ (max ordinals) + 1
            where
              max [] = 0
              max xs = maximum xs
