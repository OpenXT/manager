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

module Core.Types
       (
         URI (..)
       , Text
       , DBPath
       , DBusValue (..)
       , DBusKeyValue (..)
       , PtRule (..)
       , FileContents
       , FileResource (..)
       , DBEntry (..)
       , DBSection (..)
       , DBValue
       , EnvFile (..)
       , DomStoreFile (..)
       , FilesystemType (..)
       , filesystemFromStr
       , filesystemStr
       , parseURI
       , strFileResource
       , module Core.Uuid
       ) where

import Data.Text.Lazy (Text)
import Data.List
import Data.Char
import Data.Maybe
import System.FilePath
import Network.DBus (DBusValue)
import Core.Uuid

type FileContents = Text
data DBusKeyValue = DBusKeyValue String DBusValue

data URI = URI { uriScheme :: String, uriLocation :: String } deriving (Eq)

instance Show URI where
  show (URI s l) = if null s then l else s ++ ":" ++ l

parseURI :: String -> Maybe URI
parseURI str
  = maybe (Just $ URI "" str) Just $
      safeHead . map fromJust . filter isJust $ [ prefix "ovf", prefix "http", prefix "https", prefix "file" ]
  where
    prefix p | (p++":") `isPrefixOf` str = Just (URI p (drop (length p + 1) str))
             | otherwise = Nothing
    safeHead [] = Nothing
    safeHead (x:_) = Just x

data FileResource
   = FileResource URI
     deriving Eq

instance Show FileResource where show (FileResource uri) = show uri

strFileResource (FileResource uri) = show uri

data DBEntry
   = DBEntry
     { dbeSection :: DBSection
     , dbePath :: DBPath
     , dbeValue :: DBValue }
 deriving (Eq,Show)

type DBPath = String
type DBValue = String

data DBSection = DomStoreSection | VmSection
 deriving (Eq,Show)

data EnvFile = EnvFile { efFile :: FileResource, efRelativePath :: FilePath } deriving (Eq, Show)
               
data DomStoreFile = DomStoreFile FileResource FilePath
 deriving (Eq,Show)

data PtRule
   = PtMatchID { ptClass :: Maybe Int
               , ptVendorID :: Maybe Int
               , ptDeviceID :: Maybe Int }
   | PtMatchBDF { ptBDF :: String }
     deriving (Eq, Show)

data FilesystemType = Ext2 | Ext3 | Ext4 | Minix | Ntfs | Swap deriving (Eq, Show)

filesystemStr Ext2 = "ext2"
filesystemStr Ext3 = "ext3"
filesystemStr Ext4 = "ext4"
filesystemStr Minix = "minix"
filesystemStr Ntfs = "ntfs"
filesystemStr Swap = "swap"
filesystemFromStr str = f str where
  f "ext2" = Just Ext2
  f "ext3" = Just Ext3
  f "ext4" = Just Ext4
  f "minix" = Just Minix
  f "ntfs" = Just Ntfs
  f "swap" = Just Swap
  f _ = Nothing
