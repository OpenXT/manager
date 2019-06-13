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

module OVF.ModelXCI
       (
         XCIPropertyOverride (..)
       , XCIAppliance (..)
       , XCIDisk (..)
       , XCINetwork (..)
       , XCIVm (..)
       , XCINetworkAdapter (..)
       , XCIStorageDevice (..)
       , getXciDisk
       , getXciNetwork
       , getXciVm
       , getXciNetworkAdapter
       , getXciStorageDevice
       ) where

import OVF.Model
import Core.Types
import Data.List


getXciDisk app id = find ((== id) . xciDiskId) (xciAppDisks app)
getXciNetwork app name = find ((== name) . xciNetworkName) (xciAppNetworks app)
getXciVm app id = find ((== id) . xciVmId) (xciAppVms app)
getXciNetworkAdapter vm id = find ((== id) . xciNetworkAdapterId) (xciVmNetworkAdapters vm)
getXciStorageDevice vm id = find ((== id) . xciStorageDeviceId) (xciVmStorageDevices vm)

-- Arbitrary property override
-- values as strings, will be converted to DBusValue by dbus parser at later stages (with help from IDL)
data XCIPropertyOverride
   = XCIPropertyOverride { xciPropertyName :: String
                         , xciPropertyValue :: String }
     deriving (Eq, Show)


data XCIAppliance
   = XCIAppliance
     {
       xciAppID :: Maybe String
     , xciAppVersion :: Maybe Int
     , xciAppDisks :: [XCIDisk]
     , xciAppNetworks :: [XCINetwork]
     , xciAppVms :: [XCIVm]
     } deriving (Eq, Show)


data XCIDisk
   = XCIDisk
     {
       xciDiskId :: DiskID
     , xciDiskEncryption :: DiskEncryption
     , xciDiskFilesystem :: Maybe FilesystemType
     } deriving (Eq, Show)

data XCINetwork
   = XCINetwork
     {
       xciNetworkName :: String
     , xciNetworkClientId :: Maybe String
     } deriving (Eq, Show)

data XCIVm
   = XCIVm
     { 
       xciVmId :: SystemID
     , xciVmUuid :: Maybe Uuid
     , xciVmTemplate :: Maybe String
     , xciVmPropertyOverride :: [XCIPropertyOverride]
     , xciVmArgoRules :: [String]
     , xciVmRpcRules :: [String]
     , xciVmPtRules :: [PtRule]
     , xciVmDB :: [DBEntry]
     , xciVmDomStoreFiles :: [FileID]
     , xciVmNetworkAdapters :: [XCINetworkAdapter]
     , xciVmStorageDevices :: [XCIStorageDevice]
     } deriving (Eq, Show)
       
data XCINetworkAdapter
   = XCINetworkAdapter
     {
       xciNetworkAdapterId :: ResInstanceID
     , xciNetworkAdapterPropertyOverride :: [XCIPropertyOverride]
     } deriving (Eq, Show)
     
data XCIStorageDevice
   = XCIStorageDevice
     {
       xciStorageDeviceId :: ResInstanceID
     , xciStorageDevicePropertyOverride :: [XCIPropertyOverride]
     } deriving (Eq, Show)

