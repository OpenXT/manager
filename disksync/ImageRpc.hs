--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

module ImageRpc ( expose, unexpose )
    where

import Data.String

import Tools.Misc
import Tools.Log

import Rpc
import Rpc.Autogen.DisksyncmgrDiskimageServer
import Db
import Image
import Types
import DiskSync
import ObjectPaths

expose :: AppContext -> ImageId -> Rpc ()
expose ctx image_id =
    do rpcExpose path (interfaces $ implementationFor ctx image_id)
       info $ "exposed image " ++ show image_id ++ " over DBUS at " ++ show path
    where
      path = imageObjPath image_id

unexpose :: ImageId -> Rpc ()
unexpose image_id =
    do rpcHide path
       info $ "removed task " ++ show image_id ++ " from DBUS at " ++ show path
    where
      path = imageObjPath image_id

implementationFor ctx image_id = self where
  self =
      DisksyncmgrDiskimageServer
      {
        comCitrixXenclientDisksyncmgrImageGetId = return $ show image_id
      , comCitrixXenclientDisksyncmgrImageGetDiskId = get_str "disk-id"
      , comCitrixXenclientDisksyncmgrImageGetImageUuid = get_str "image-uuid"
      , comCitrixXenclientDisksyncmgrImageGetImageUrl = get_str "image-url"
      , comCitrixXenclientDisksyncmgrImageGetTransferUrl = get_str "transfer-url"
      , comCitrixXenclientDisksyncmgrImageGetServerCertPath = get_str "server-cert-path"
      , comCitrixXenclientDisksyncmgrImageGetClientCertPath = get_str "client-cert-path"
      , comCitrixXenclientDisksyncmgrImageGetClientKeyPath = get_str "client-key-path"
      , comCitrixXenclientDisksyncmgrImageGetEncryptLocalImage = get_bool "encrypt-local-image"
      , comCitrixXenclientDisksyncmgrImageGetTransferContextId = get_str "transfer-context-id"
      }
      where
        get_bool :: String -> Rpc Bool
        get_bool prop = return . boolOfString =<< dbReadWithDefault ( imageDBPath image_id ++ "/" ++ prop ) "False"

        get_str :: String -> Rpc String
        get_str prop = dbReadWithDefault ( imageDBPath image_id ++ "/" ++ prop ) ""
