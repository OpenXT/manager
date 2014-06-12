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

module XenMgr.Expose.ObjectPaths
    ( vmObjPath, nicObjectPath, diskObjectPath, hostObjectPath
    , xenmgrObjectPath
    ) where

import Rpc.Core
import Vm.Types
import Data.String

vmObjPath :: Uuid -> ObjectPath
vmObjPath uuid = fromString $ "/vm/" ++ uuidStrUnderscore uuid

nicObjectPath :: Uuid -> NicID -> ObjectPath
nicObjectPath vm_uuid (XbDeviceID nic_id) = fromString $
    "/vm/" ++ uuidStrUnderscore vm_uuid ++ "/nic/" ++ show nic_id

diskObjectPath :: Uuid -> DiskID -> ObjectPath
diskObjectPath vm_uuid disk_id = fromString $
    "/vm/" ++ uuidStrUnderscore vm_uuid ++ "/disk/" ++ show disk_id

hostObjectPath :: ObjectPath
hostObjectPath = fromString "/host"

xenmgrObjectPath :: ObjectPath
xenmgrObjectPath = fromString "/"

