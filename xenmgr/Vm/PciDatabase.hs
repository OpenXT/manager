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

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Vm.PciDatabase
	( PciDatabase
	, VendorId
	, DeviceId
        , DeviceEntry(..)
        , Name
	, empty
	, load
	, loadDefault
        , gatherPciDeviceEntries
	) where

import Control.Applicative ((<*), (*>), (<|>), (<$>), (<*>), pure)
import Control.Monad
import Data.Attoparsec
import Data.Attoparsec.Combinator
--import qualified Data.Attoparsec.Lazy as AL
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString)
import qualified Data.List
import Data.Word
import Prelude hiding (concat, foldl, take, readFile, takeWhile)
import Text.Printf
import Directory (removeFile)
import Tools.Process (safeSpawnShell)
import qualified Codec.Compression.GZip as GZ

data PciDatabase = PciDatabase [Vendor] [Class]

empty = PciDatabase [] []

data Vendor    = Vendor    VendorId    Name [Device]    deriving Show
data Device    = Device    DeviceId    Name [SubSystem] deriving Show
data SubSystem = SubSystem SubSystemId Name             deriving Show
data Class     = Class     ClassId     Name [SubClass]  deriving Show
data SubClass  = SubClass  ClassId     Name [Interface] deriving Show
data Interface = Interface InterfaceId Name             deriving Show

type Name        = BS.ByteString
type VendorId    = Id4
type DeviceId    = Id4
type ClassId     = Id2
type InterfaceId = Id2
type SubSystemId = (VendorId, DeviceId)

newtype Id2 = Id2 Word8  deriving (Eq, Num)
newtype Id4 = Id4 Word16 deriving (Eq, Num)

instance Show Id2 where show (Id2 x) = printf "%02x" x
instance Show Id4 where show (Id4 x) = printf "%04x" x

pciDatabase = PciDatabase <$> vendors <*> pure [] {- we don't need classes for now -}

vendors = ignore *> manyTill vendor (word8 eol)
classes = ignore *> many class_

vendor    = liftM3 Vendor    vendorId    name (many device)
device    = liftM3 Device    deviceId    name (many subSystem)
subSystem = liftM2 SubSystem subSystemId name
class_    = liftM3 Class     classId     name (many subClass)
subClass  = liftM3 SubClass  subClassId  name (many interface)
interface = liftM2 Interface interfaceId name

ignore = skipMany $ emptyLine <|> commentLine

name = skipWhile (== space) *> takeWhile (/= eol) <* take 1 <* skipMany commentLine
commentLine = word8 hash *> skipWhile (/= eol) <* take 1
emptyLine = skip (== eol)

id2 = liftM (Id2 . valueOfHexString) $ take 2
id4 = liftM (Id4 . valueOfHexString) $ take 4

valueOfHexString :: (Bits a, Num a) => BS.ByteString -> a
valueOfHexString = BS.foldl (\a b -> a `shiftL` 4 + (fromIntegral $ valueOfHexByte b)) 0
valueOfHexByte b = if b .&. 0x40 == 0 then b - 0x30 else b - 0x57

vendorId    = id4
deviceId    = s "\t" *> id4
classId     = s "C " *> id2
subClassId  = s "\t" *> id2
interfaceId = s "\t\t" *> id2
subSystemId = liftM2 (,) (s "\t\t" *> id4) (s " " *> id4)

s = string . fromString

eol   = 0x0a
hash  = 0x23
space = 0x20

{-
parseLazy :: BL.ByteString -> Either String PciDatabase
parseLazy x =
	case AL.parse pciDatabase x of
		AL.Fail _ _ error -> Left error
		AL.Done _ result -> Right result
-}

parseStrict :: BS.ByteString -> Either String PciDatabase
parseStrict = parseOnly pciDatabase

defaultLocation = "/usr/share/pci.ids.gz"

-- TODO: Use Error Monad
--       Parsing could be Lazy as well.
load :: FilePath -> IO (Either String PciDatabase)
load path = do
    content <- fmap GZ.decompress (BL.readFile path)
    return $ parseStrict (BS.concat $ BL.toChunks content)

loadDefault = load defaultLocation

data DeviceEntry
   = DeviceEntry { deId :: (VendorId, DeviceId)
                 , deVendorName :: Name
                 , deDeviceName :: Name }
     deriving (Eq, Show)

allPciDeviceEntries :: PciDatabase -> [DeviceEntry]
allPciDeviceEntries (PciDatabase vendors _) =
  Data.List.concat $ map vendorDevices vendors where
    vendorDevices (Vendor vendorId vendorName devices)
     = map mkEntry devices where
       mkEntry (Device devId devName _) = DeviceEntry (vendorId, devId) vendorName devName

gatherPciDeviceEntries :: PciDatabase -> [(VendorId, DeviceId)] -> [DeviceEntry]
gatherPciDeviceEntries db ids =
  truncate (allPciDeviceEntries db) where
    truncate = filter (\entry -> deId entry `elem` ids)
