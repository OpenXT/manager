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

module VirtualSystem
       (
         VirtualSystem(..)
       , VirtualSystemID (..)
       , VirtualSystemCollection (..)
       , CollectionID (..)
       , Content (..)
       , TemplateID (..)
       , DBEntry (..)
       , DBValue (..)
       , DBSection (..)
       , ArgoFirewallRule
       , RpcFirewallRule
       , DomStoreFile (..)
       , NIC (..)
       , Disk (..)
       , DiskEncryption (..)
       , DiskID (..)
       , DiskImageType (..)
       , DiskImageID (..)
       , DiskImage (..)
       , DiskAccess (..)
       , DBusProperty (..)
       , ProductProperty (..)
       , ProductPropertyType (..)
       , ProductPropertyValue
       , catPropertyOverrides
       )
       where

import Data.List
import Core.Types
import Core.Uuid
import Vm.ProductProperty

data VirtualSystem
   = VirtualSystem
     {
       vsID :: VirtualSystemID
     , vsUuid :: Maybe Uuid
     , vsName :: String
     , vsTemplate :: Maybe TemplateID
     , vsTransportIso :: Bool
     , vsMemory :: Int
     , vsVcpus :: Int
     , vsInstallBootStopDelay :: Maybe Int
     , vsPropertyOverrides :: [DBusProperty]
     , vsProductProperties :: [ProductProperty]
     , vsDisks :: [Disk]
     , vsNICs :: [NIC]
     , vsPciPt :: [PtRule]
     , vsArgoFirewall :: [ArgoFirewallRule]
     , vsRpcFirewall :: [RpcFirewallRule]
     , vsDB :: [DBEntry]
     , vsEnvFiles :: [EnvFile]
     , vsDomStoreFiles :: [DomStoreFile]
     } deriving (Eq,Show)

data VirtualSystemCollection
   = VirtualSystemCollection
     {
       collectionID :: CollectionID
     , collectionInfo :: String
     , collectionName :: String
     , collectionProductProperties :: [ProductProperty]
     , collectionItems :: [Content]
     } deriving (Eq, Show)

data Content = ContentVirtualSystem VirtualSystem
             | ContentVirtualSystemCollection VirtualSystemCollection
               deriving (Eq,Show)

newtype VirtualSystemID = VirtualSystemID String deriving Eq
newtype CollectionID = CollectionID String deriving (Eq,Show)
newtype TemplateID = TemplateID String deriving (Eq,Show)

instance Show VirtualSystemID where show (VirtualSystemID s) = s

type ArgoFirewallRule = String
type RpcFirewallRule = String

data NIC
   = NIC { nicID :: Int
         , nicEnabled :: Bool
         , nicNetwork :: String
         , nicPropertyOverrides :: [DBusProperty]
         }
 deriving (Eq,Show)

data DiskID = DiskID VirtualSystemID Int deriving (Eq, Show)
newtype DiskImageID = DiskImageID String deriving (Eq)

instance Show DiskImageID where show (DiskImageID str) = str

data DiskAccess = DiskAccessRead | DiskAccessWrite | DiskAccessReadWrite
  deriving (Eq,Show)

data DiskImageType = ISO | VHD | CPIO | RawFilesystem deriving (Eq,Show)

data DiskImage
   = DiskImage { diID :: DiskImageID
               , diType :: DiskImageType
               , diShared :: Bool
               , diCapacity :: Integer
               , diEncryption :: DiskEncryption
               , diFilesystem :: Maybe FilesystemType
                 -- if not specified, typically empty disk will be created
               , diFile :: Maybe FileResource }
     deriving (Eq, Show)

data Disk
   = Disk { diskID :: DiskID
          , diskEnabled :: Bool
          , diskIsCdrom :: Bool
          , diskAccess :: DiskAccess
          , diskImage :: Maybe DiskImage
          , diskPropertyOverrides :: [DBusProperty]
          }
 deriving (Eq,Show)

data DiskEncryption
   = NoEncryption
   | GenerateCryptoKey Int
   | UseCryptoKey FileResource
     deriving (Eq, Show)

data DBusProperty
   = DBusProperty
     { dbusPropertyName :: String
     , dbusPropertyInterface :: String
     , dbusPropertyValue :: DBusValue }
 deriving (Eq,Show)

addPropertyOverride :: DBusProperty -> [DBusProperty] -> [DBusProperty]
addPropertyOverride p ps =
  case partition (matches p) ps of
    (m, other) -> other ++ [p]
  where    
    matches p p' = dbusPropertyName p == dbusPropertyName p' && dbusPropertyInterface p == dbusPropertyInterface p'

-- catPropertyOverrides a b - properties in set b take precedence over a
catPropertyOverrides :: [DBusProperty] -> [DBusProperty] -> [DBusProperty]
catPropertyOverrides a b = foldr addPropertyOverride a b


