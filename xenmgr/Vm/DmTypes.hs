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

{-# LANGUAGE TypeSynonymInstances #-}
module Vm.DmTypes
       ( DomainID
       , XbDeviceID (..)
       , DiskID
       , DiskMap
       , DiskMode (..)
       , DiskType (..)
       , DiskSnapshotMode (..)
       , DiskDeviceType (..)
       , ManagedDiskType (..)
       , Disk (..)
       , Sha1Sum
       , NicDef (..)
       , NicDefMap
       , NicID
       , Mac
       , macToBytes, bytesToMac
       , Nic (..)
       , NetworkInfo (..)
       , Network
       , networkObjectPath
       , networkToStr, networkFromStr, networkStateFromStr
       , fallbackNetwork
       , isCdrom
       )
       where

import Data.Int
import Data.String
import Data.List (intersperse)
import Data.Char (digitToInt)
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import Text.Printf
import Vm.Uuid
import Tools.Misc
import Rpc.Core (ObjectPath, mkObjectPath_, strObjectPath)

type DomainID = Int32

-- | xenbus device ID
newtype XbDeviceID = XbDeviceID { xbdevID :: Int }
    deriving (Eq,Ord)

instance Show XbDeviceID where
    show d = show (xbdevID d)
instance IsString XbDeviceID where
  fromString x = XbDeviceID (fromString x)

-- | Disk definitions
type DiskID   = Int
type DiskMap  = M.Map DiskID Disk

instance IsString DiskID where fromString = read

data DiskMode = ReadOnly
              | ReadWrite
                deriving (Eq, Show)

data DiskType = DiskImage
              | PhysicalDevice
              | QemuCopyOnWrite
              | VirtualHardDisk
              | ExternalVdi
              | Aio
              | Raw
                deriving (Eq, Show)

data DiskSnapshotMode = SnapshotTemporary
                      | SnapshotTemporaryEncrypted
                      | SnapshotCoalesce
                      | SnapshotScripted
                      | SnapshotScriptedAuthor
                      | SnapshotScriptedNoSnapshot
                        deriving (Eq, Show)

data DiskDeviceType = DiskDeviceTypeDisk
                    | DiskDeviceTypeCdRom
                      deriving (Eq, Show)

data ManagedDiskType = UnmanagedDisk | ApplicationDisk | UserDisk | SystemDisk
                     deriving (Eq, Show)

-- Virtual disk specification
data Disk = Disk { diskPath :: FilePath
                 , diskType :: DiskType
                 , diskMode :: DiskMode
                 , diskDevice :: String
                 , diskDeviceType :: DiskDeviceType
                 , diskSnapshotMode :: Maybe DiskSnapshotMode
                 , diskSha1Sum :: Maybe Sha1Sum
                 , diskShared :: Bool
                 , diskEnabled :: Bool
                 , diskManagedType :: ManagedDiskType
                 } deriving (Eq, Show)

type Sha1Sum = Integer

-- NIC definition in database
data NicDef = NicDef { nicdefId :: NicID
                     , nicdefNetwork :: Network
                     , nicdefWirelessDriver :: Bool
                     , nicdefBackendUuid :: Maybe Uuid
                     , nicdefBackendName :: Maybe String
                     , nicdefBackendDomid :: Maybe DomainID
                     , nicdefEnable :: Bool
                     , nicdefMac :: Maybe String
                     } deriving (Eq, Show)

type NicDefMap = M.Map NicID NicDef
type NicID  = XbDeviceID
type Mac    = String

-- Actual NIC as plugged in
data Nic = Nic { nicId :: NicID
               , nicNetwork :: Network
               , nicMac :: Mac
               } deriving (Eq, Show)

-- | path references the network object hold within networking daemon
newtype Network = Network ObjectPath deriving (Eq, Show)

networkObjectPath :: Network -> ObjectPath
networkObjectPath (Network n) = n

data NetworkInfo
   = NetworkInfo {
       niHandle :: Network
     , niName :: String
     , niBridgeName :: String
     , niBackend :: Uuid
     , niIsWireless :: Bool
     , niIsShared :: Bool
     , niIsInternal :: Bool
     , niIsConfigured :: Bool
     , niCarrier :: Bool
     } deriving (Eq,Show)

fallbackNetwork :: Network
fallbackNetwork = Network (fromString "/wired/0/bridged")

networkToStr :: Network -> String
networkToStr (Network p) = TL.unpack (strObjectPath p)

networkFromStr :: String -> Network
networkFromStr s = Network (mkObjectPath_ $ fromString $ legacyNNames s)

networkStateFromStr :: String -> Int
networkStateFromStr s = read s :: Int

-- TODO: ideally this could be handled somewhere in the upgrade process
legacyNNames "brbridged" = "/wired/0/bridged"
legacyNNames "brshared" = "/wired/0/shared"
legacyNNames "brwireless" = "/wifi/0/shared"
legacyNNames "brinternal" = "/internal"
legacyNNames x = x


isCdrom :: Disk -> Bool
isCdrom disk = diskDevice disk == "hdc"

macToBytes :: Mac -> [Int]
macToBytes = map hexify . (split ':')
    where hexify [a,b] = 0x10 * digitToInt a + digitToInt b
          hexify _     = error "bad mac string"

bytesToMac :: [Int] -> Mac
bytesToMac = concat . intersperse ":" . map stringify
    where stringify x = printf "%02x" x
