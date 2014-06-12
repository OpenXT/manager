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

module ObjectPaths
    ( diskGroupObjPath
    , diskObjPath
    , imageObjPath
    , diskSyncDBPath
    , diskGroupDBPath
    , diskGroupsDBPath
    , diskDBPath
    , disksDBPath
    , imageDBPath
    , imagesDBPath
    ) where

import Data.String
import Tools.Misc
import Rpc
import Types


-----------------
-- DBus ObjectPaths
-----------------
uuidObjPathStr :: (Show a) => a -> String
uuidObjPathStr uuid = replace "-" "_" (show uuid)

diskGroupObjPathStr :: GroupId -> String
diskGroupObjPathStr group_id = "/diskgroup/" ++ uuidObjPathStr group_id

diskObjPathStr :: DiskId -> String
diskObjPathStr disk_id  = "/disk/" ++ uuidObjPathStr disk_id

imageObjPathStr :: ImageId -> String
imageObjPathStr image_id = "/image/" ++ uuidObjPathStr image_id

diskGroupObjPath :: GroupId -> ObjectPath
diskGroupObjPath = fromString . diskGroupObjPathStr

diskObjPath :: DiskId -> ObjectPath
diskObjPath = fromString . diskObjPathStr

imageObjPath :: ImageId -> ObjectPath
imageObjPath = fromString . imageObjPathStr

-----------------
-- DB paths
-----------------
diskSyncDBPath :: String
diskSyncDBPath = "/disksyncmgr"

diskGroupsDBPath :: String
diskGroupsDBPath = diskSyncDBPath ++ "/group"

diskGroupDBPath :: GroupId -> String
diskGroupDBPath group_id = diskGroupsDBPath ++ "/" ++ show group_id

disksDBPath :: String
disksDBPath = diskSyncDBPath ++ "/disk"

diskDBPath :: DiskId -> String
diskDBPath disk_id = disksDBPath ++ "/" ++ show disk_id

imagesDBPath :: String
imagesDBPath = diskSyncDBPath ++ "/image"

imageDBPath :: ImageId -> String
imageDBPath image_id = imagesDBPath ++ "/" ++ show image_id
