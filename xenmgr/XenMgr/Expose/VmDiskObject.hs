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

module XenMgr.Expose.VmDiskObject (expose, unexpose) where

import Control.Applicative
import Rpc.Autogen.VmDiskServer
import XenMgr.Rpc
import XenMgr.Errors
import Vm.Types
import Vm.Queries
import Vm.Actions
import Vm.Config
import Tools.Log
import Data.Int
import Data.Maybe
import Data.String
import XenMgr.Expose.ObjectPaths
import XenMgr.Db
import XenMgr.XM
import Vm.Monad

data ID = ID { vm_uuid :: Uuid
             , disk_id :: DiskID }

expose :: Uuid -> DiskID -> XM ()
expose vm_uuid disk_id = do
    let path = diskObjectPath vm_uuid disk_id
    info $ "exposing disk object " ++ (show path)
    xm <- xmContext
    let imp = implementation xm (ID vm_uuid disk_id)
    liftRpc $ rpcExpose path (interfaces imp)

unexpose :: Uuid -> DiskID -> Rpc ()
unexpose vm_uuid disk_id = do
    let path = diskObjectPath vm_uuid disk_id
    info $ "removing disk object " ++ (show path)
    rpcHide path

implementation :: XmContext -> ID -> VmDiskServer Rpc
implementation xm id@(ID vm diskID) = self where
  runvm = runXM xm . xmRunVm vm
  self =  VmDiskServer {
    comCitrixXenclientVmdiskAttachVhd = runvm . _AttachVhd id
  , comCitrixXenclientVmdiskAttachPhy = runvm . _AttachPhy id
  , comCitrixXenclientVmdiskMount = _Mount id
  , comCitrixXenclientVmdiskUmount = _Unmount id
  , comCitrixXenclientVmdiskDelete = runvm $ _Delete id
  , comCitrixXenclientVmdiskGetBackendUuid = _GetBackendUuid id
  , comCitrixXenclientVmdiskSetBackendUuid = runvm . _SetBackendUuid id
  , comCitrixXenclientVmdiskGetBackendName = _GetBackendName id
  , comCitrixXenclientVmdiskSetBackendName = runvm . _SetBackendName id
  , comCitrixXenclientVmdiskGetPhysPath = _GetPhysPath id
  , comCitrixXenclientVmdiskSetPhysPath = runvm . _SetPhysPath id
  , comCitrixXenclientVmdiskGetPhysType = _GetPhysType id
  , comCitrixXenclientVmdiskSetPhysType = runvm . _SetPhysType id
  , comCitrixXenclientVmdiskGetManagedDisktype = _GetManagedDiskType id
  , comCitrixXenclientVmdiskSetManagedDisktype = runvm . _SetManagedDiskType id
  , comCitrixXenclientVmdiskGetVirtPath = _GetVirtPath id
  , comCitrixXenclientVmdiskSetVirtPath = runvm . _SetVirtPath id
  , comCitrixXenclientVmdiskGetMode = _GetMode id
  , comCitrixXenclientVmdiskSetMode = runvm . _SetMode id
  , comCitrixXenclientVmdiskGetDevtype = _GetDevtype id
  , comCitrixXenclientVmdiskSetDevtype = runvm . _SetDevtype id
  , comCitrixXenclientVmdiskGetSnapshot = _GetSnapshot id
  , comCitrixXenclientVmdiskSetSnapshot = runvm . _SetSnapshot id
  , comCitrixXenclientVmdiskGetShared = _GetShared id
  , comCitrixXenclientVmdiskSetShared = runvm . _SetShared id
  , comCitrixXenclientVmdiskGetEnabled = _GetEnabled id
  , comCitrixXenclientVmdiskSetEnabled = runvm . _SetEnabled id
  , comCitrixXenclientVmdiskGetEncryptionKeySet = _GetEncryptionKeySet id
  , comCitrixXenclientVmdiskGetVirtualSizeMb = _GetVirtualSizeMb id
  , comCitrixXenclientVmdiskGetUtilizationBytes = _GetPhysicalUtilizationBytes id
  , comCitrixXenclientVmdiskGenerateCryptoKey = \keybits -> generateCryptoKey vm diskID (fromIntegral keybits)
  , comCitrixXenclientVmdiskGenerateCryptoKeyIn = \keybits path -> generateCryptoKeyIn vm diskID (fromIntegral keybits) path
  }

_AttachVhd :: ID -> String -> Vm ()
_AttachVhd id path =
    _modify_disk id $
         \d -> d { diskPath = path
                 , diskType = VirtualHardDisk
                 , diskMode = Vm.Types.ReadWrite
                 , diskDeviceType = DiskDeviceTypeDisk }

_AttachPhy :: ID -> String -> Vm ()
_AttachPhy id path =
    _modify_disk id $
         \d -> d { diskPath = path
                 , diskType = PhysicalDevice
                 , diskMode = Vm.Types.ReadWrite
                 , diskDeviceType = DiskDeviceTypeDisk }

_Mount :: ID -> String -> Bool -> Rpc ()
_Mount (ID uuid disk_id) path readonly = mountVmDisk uuid disk_id readonly path

_Unmount :: ID -> Rpc ()
_Unmount (ID uuid disk_id) = unmountVmDisk uuid disk_id

_Delete :: ID -> Vm ()
_Delete (ID vm_uuid disk_id) =
    do removeDiskFromVm vm_uuid disk_id
       liftRpc $ unexpose vm_uuid disk_id

_GetBackendUuid :: ID -> Rpc String
_GetBackendUuid _ = return ""

_SetBackendUuid :: ID -> String -> Vm ()
_SetBackendUuid _ _ =
    error "not implemented"

_GetBackendName :: ID -> Rpc String
_GetBackendName _ = return ""

_SetBackendName :: ID -> String -> Vm ()
_SetBackendName _ _ =
    error "not implemented"

_GetPhysPath :: ID -> Rpc (String)
_GetPhysPath id = _get_field id diskPath

_SetPhysPath :: ID -> String -> Vm ()
_SetPhysPath id path = _modify_disk id $ \d -> d { diskPath = path }

_GetPhysType :: ID -> Rpc (String)
_GetPhysType id = _get_field id (enumMarshall . diskType)

_SetPhysType :: ID -> String -> Vm ()
_SetPhysType id typ = _modify_disk id $ \d -> d { diskType = enumMarshallReverse_ typ }

_GetManagedDiskType :: ID -> Rpc (String)
_GetManagedDiskType id = _get_field id (enumMarshall . diskManagedType)

_SetManagedDiskType :: ID -> String -> Vm ()
_SetManagedDiskType id typ = _modify_disk id $ \d -> d { diskManagedType = enumMarshallReverse_ typ }

_GetVirtPath :: ID -> Rpc (String)
_GetVirtPath id = _get_field id diskDevice

_SetVirtPath :: ID -> String -> Vm ()
_SetVirtPath id path = _modify_disk id $ \d -> d { diskDevice = path }

_GetMode :: ID -> Rpc (String)
_GetMode id = _get_field id (enumMarshall . diskMode)

_SetMode :: ID -> String -> Vm ()
_SetMode id mode = _modify_disk id $ \d -> d { diskMode = enumMarshallReverse_ mode }

_GetShared :: ID -> Rpc Bool
_GetShared id = _get_field id diskShared

_SetShared :: ID -> Bool -> Vm ()
_SetShared id sh = _modify_disk id $ \d -> d { diskShared = sh }

_GetEnabled :: ID -> Rpc Bool
_GetEnabled id = _get_field id diskEnabled

_SetEnabled :: ID -> Bool -> Vm ()
_SetEnabled id sh = _modify_disk id $ \d -> d { diskEnabled = sh }

_GetDevtype :: ID -> Rpc (String)
_GetDevtype id = _get_field id (enumMarshall . diskDeviceType)

_SetDevtype :: ID -> String -> Vm ()
_SetDevtype id typ = _modify_disk id $ \d -> d { diskDeviceType = enumMarshallReverse_ typ }

_GetSnapshot :: ID -> Rpc (String)
_GetSnapshot id =
    do s <- _get_field id diskSnapshotMode
       case s of
         Just s  -> return $ enumMarshall s
         Nothing -> return "none"

_SetSnapshot :: ID -> String -> Vm ()
_SetSnapshot id "none" = _modify_disk id $ \d -> d { diskSnapshotMode = Nothing }
_SetSnapshot id s      = do
                            enc <- liftRpc $ _GetEncryptionKeySet id
                            if enc
                              then
                                _modify_disk id $ \d -> d { diskSnapshotMode = Just $ enumMarshallReverse_ "temporary-encrypted" }
                              else
                                _modify_disk id $ \d -> d { diskSnapshotMode = Just $ enumMarshallReverse_ s }


_GetEncryptionKeySet :: ID -> Rpc Bool
_GetEncryptionKeySet (ID vm disk) = getVmDiskEncryptionKeySet vm disk

_GetVirtualSizeMb :: ID -> Rpc Int32
_GetVirtualSizeMb (ID vm disk) = fromIntegral <$> getVmDiskVirtualSizeMB vm disk

_GetPhysicalUtilizationBytes :: ID -> Rpc Int64
_GetPhysicalUtilizationBytes (ID vm disk) = fromIntegral <$> getVmDiskPhysicalUtilizationBytes vm disk

-- some helpers
_get_field (ID vm_uuid disk_id) f =
    do disk <- getDisk vm_uuid disk_id
       case disk of
         Nothing   -> failNoSuchDisk
         Just disk -> return (f disk)

_modify_disk (ID vm_uuid disk_id) f =
    modifyVmDisk vm_uuid disk_id f


