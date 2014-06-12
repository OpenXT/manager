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

module OVF.Model
       (
         FileID (..)
       , DiskID (..)
       , SystemID (..)
       , OsID (..)
       , ResInstanceID (..)
         
       , Envelope (..)
       , Content (..)
       , FileRef (..)
       , DiskSection (..)
       , Disk (..)
       , DiskEncryption (..)
       , NetworkSection (..)
       , Network (..)
       , VirtualSystem (..)
       , ProductSection (..)
       , ProductProperty (..)
       , PPType (..)
       , OSSection (..)
       , Item (..)
       , ResourceItem (..)
       , EthernetPortItem (..)
       , StorageItem (..)
       , ResourceType (..)
       , getResourceType
       , getResourceTypeID

       , resourcedVcpus
       , resourcedMemoryMB
       , collectResources
       )
       where

import Data.Map (Map)
import qualified Data.Map as Map
import OVF.AllocationUnit
import Data.Word
import Data.Int
import Core.Types

newtype FileID = FileID String deriving (Eq,Show)
newtype DiskID = DiskID String deriving (Eq,Show)
newtype SystemID = SystemID String deriving (Eq,Show)
newtype OsID = OsID String deriving (Eq,Show)

data Envelope
   = Envelope { references :: [FileRef]
              , diskSections :: [DiskSection]
              , networkSection :: Maybe NetworkSection
              , content :: Content
              , eulas :: [String]
              }
     deriving (Eq,Show)

data Content
   = ContentVirtualSystemCollection
     { collectionID :: String
     , collectionInfo :: String
     , collectionName :: String
     , collectionProductSections :: [ProductSection]
     , collectionItems :: [Content] }
   | ContentVirtualSystem
     { contentSystem :: VirtualSystem }
     deriving (Eq,Show)

data FileRef
   = FileRef { fileID :: FileID
             , fileHref :: String
             , fileSize :: Integer }
     deriving (Eq,Show)

data DiskSection
   = DiskSection { diskSectionInfo :: String
                 , disks :: [Disk] }
     deriving (Eq,Show)

data Disk
   = Disk { diskID :: DiskID
          , diskFileRef :: Maybe FileID
          , diskCapacity ::  Integer
          , diskCapacityAllocationUnits :: AllocationUnit
          , diskPopulatedSize :: Maybe Integer
          , diskShared :: Bool
          , diskFormat :: String }
     deriving (Eq,Show)

data DiskEncryption
   = NoEncryption
   | GenerateCryptoKey Int
   | UseCryptoKey FileID
     deriving (Eq, Show)

data NetworkSection
   = NetworkSection { networkSectionInfo :: String
                    , networks :: [Network] }
     deriving (Eq,Show)

data Network
   = Network { networkName :: String
             , networkDescription :: String }
     deriving (Eq,Show)

data VirtualSystem
   = VirtualSystem { systemID :: SystemID
                   , systemInfo :: String
                   , systemName :: String
                   , systemEnvFiles :: [(FileID, FilePath)]
                   , systemProductSections :: [ProductSection]
                   , systemOSSection :: [OSSection]
                   , systemResourceItems :: [ResourceItem]
                   , systemStorageItems :: [StorageItem]                   
                   , systemEthernetPortItems :: [EthernetPortItem]
                   , systemInstall :: Bool
                   , systemInstallDelay :: Int
                   , systemTransport :: [String]
                   }
     deriving (Eq,Show)

data ProductSection
   = ProductSection { productClass :: Maybe String
                    , productInstance :: Maybe String
                    , productInfo :: String
                    , productName :: String
                    , productVersion :: String
                    , productProperties :: [ProductProperty] }
     deriving (Eq,Show)

data ProductProperty
   = ProductProperty { propertyKey :: String
                     , propertyType :: PPType
                     , propertyValue :: String
                     , propertyUserConfigurable :: Bool
                     , propertyDescription :: String
                     , propertyPassword :: Bool }
     deriving (Eq,Show)

data PPType
   = PPT_Uint8 | PPT_Sint8 | PPT_Uint16 | PPT_Sint16 | PPT_Uint32 | PPT_Sint32 | PPT_Uint64 | PPT_Sint64
   | PPT_String | PPT_Bool | PPT_Real32 | PPT_Real64
     deriving (Eq,Show)

data OSSection
   = OSSection { osID :: OsID
               , osInfo :: String
               , osDescription :: String }
     deriving (Eq,Show)

data Item = RI ResourceItem | EPI EthernetPortItem | SRI StorageItem deriving (Eq,Show)
data ResInstanceID = ResInstanceID Integer deriving (Eq,Show)

-- CIM_ResourceAllocationSettingData
data ResourceItem
   = ResourceItem { resAddress :: String
                  , resAddressOnParent :: String
                  , resAllocationUnits :: AllocationUnit
                  , resAutomaticAllocation :: Bool
                  , resDescription :: String
                  , resConnection :: Maybe String
                  , resHostResource :: Maybe String
                  , resName :: String
                  , resInstanceID :: ResInstanceID
                  , resParent :: Maybe ResInstanceID
                  , resTypeID :: ResourceTypeID
                  , resSubType :: String
                  , resVirtualQuantity :: Integer
                  , resVirtualQuantityUnits :: AllocationUnit
                  , resReservation :: Integer
                  }
     deriving (Eq,Show)

-- CIM_EthernetPortAllocationSettingData
data EthernetPortItem
   = EthernetPortItem { ethResourceItem :: ResourceItem
                      , ethDefaultPortVID :: Maybe Word16
                      , ethDefaultPriority :: Maybe Word16
                      , ethDesiredVLANEndpointMode :: Maybe Word16
                      , ethGroupID :: Maybe Word32
                      , ethManagerID :: Maybe Word32
                      , ethNetworkPortProfileID :: Maybe String
                      , ethOtherEndpointMode :: Maybe String
                      , ethOtherNetworkPortProfileIDTypeInfo :: Maybe String
                      , ethPortCorrelationID :: Maybe String
                      , ethPortVID :: Maybe Word16
                      , ethPromiscuous :: Bool
                      , ethReceiveBandwidthLimit :: Word64
                      , ethReceiveBandwidthReservation :: Word64
                      , ethSourceMACFilteringEnabled :: Bool
                      , ethAllowedPriorities :: [Word16]
                      , ethAllowedToReceiveMACAddresses :: [String]
                      , ethAllowedToReceiveVLANs :: [Word16]
                      , ethAllowedToTransmitMACAddresses :: [String]
                      , ethAllowedToTransmitVLANs :: [Word16]
                      }
   deriving (Eq,Show)

-- CIM_StorageAllocationSettingData
data StorageItem
   = StorageItem { srResourceItem :: ResourceItem
                 , srAccess :: Word16 -- 0 not set 1 readable 2 writable 3 both
                 , srHostExtentName :: String
                 , srHostExtentNameFormat :: Word16
                 , srHostExtentNameNamespace :: Word16
                 , srHostExtentStartingAddress :: Word64
                 , srHostResourceBlockSize :: Word64
                 , srLimit :: Word64
                 , srOtherHostExtentNameFormat :: String
                 , srOtherHostExtentNameNamespace :: String
                 , srVirtualResourceBlockSize :: Word64 }
   deriving (Eq,Show)

data ResourceType
   = RT_Other
   | RT_ComputerSystem
   | RT_Processor
   | RT_Memory
   | RT_IDEController
   | RT_ParallelSCSIHBA
   | RT_FCHBA
   | RT_iSCSIHBA
   | RT_IBHCA
   | RT_EthernetAdapter
   | RT_OtherNetworkAdapter
   | RT_IOSlot
   | RT_IODevice
   | RT_FloppyDrive
   | RT_CDDrive
   | RT_DVDDrive
   | RT_HardDisk
   | RT_OtherStorageDevice
   | RT_USBController
   | RT_SoundCard
     deriving (Eq, Ord, Show)

type ResourceTypeID = Int

resourceTypeIDs :: [(ResourceType, ResourceTypeID)]
resourceTypeIDs =
  [ (RT_Other, 1)
  , (RT_ComputerSystem, 2)
  , (RT_Processor, 3)
  , (RT_Memory, 4)
  , (RT_IDEController, 5)
  , (RT_ParallelSCSIHBA, 6)
  , (RT_FCHBA, 7)
  , (RT_iSCSIHBA, 8)
  , (RT_IBHCA, 9)
  , (RT_EthernetAdapter, 10)
  , (RT_OtherNetworkAdapter, 11)
  , (RT_IOSlot, 12)
  , (RT_IODevice, 13)
  , (RT_FloppyDrive, 14)
  , (RT_CDDrive, 15)
  , (RT_DVDDrive, 16)
  , (RT_HardDisk, 17)
  , (RT_OtherStorageDevice, 20)
  , (RT_USBController, 23)
  , (RT_SoundCard, 35)
  ]

getResourceType :: ResourceTypeID -> Maybe ResourceType
getResourceType k = Map.lookup k id2ResourceType

getResourceTypeID :: ResourceType -> Maybe ResourceTypeID
getResourceTypeID k = Map.lookup k resourceType2ID

resourceType2ID :: Map ResourceType ResourceTypeID
resourceType2ID = Map.fromList resourceTypeIDs

id2ResourceType :: Map ResourceTypeID ResourceType
id2ResourceType = Map.fromList $ map (\(p,q) -> (q,p)) resourceTypeIDs

resourcedVcpus :: [ResourceItem] -> Maybe Int
resourcedVcpus
  = f . fromIntegral . sum . map resVirtualQuantity . collectResources RT_Processor
  where f 0 = Nothing
        f x = Just x

resourcedMemoryMB :: [ResourceItem] -> Maybe Int
resourcedMemoryMB
  = f . ceiling . (\x -> x / 1024 / 1024) . fromIntegral . sum . map quantityInBytes . collectResources RT_Memory
  where f 0 = Nothing
        f x = Just x

quantityInBytes :: ResourceItem -> Integer
quantityInBytes r = allocationUnitBytes (resAllocationUnits r) * resVirtualQuantity r

collectResources :: ResourceType -> [ResourceItem] -> [ResourceItem]
collectResources t = filter ((== Just t) . getResourceType . resTypeID)
