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

module Show where

import Data.List
import Text.Printf
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as UTF8
import Network.DBus

showDBus :: DBusValue -> String
showDBus (DBusByte v) = show v
showDBus (DBusBoolean True) = "true"
showDBus (DBusBoolean False) = "false"
showDBus (DBusInt16 v) = show v
showDBus (DBusUInt16 v) = show v
showDBus (DBusInt32 v) = show v
showDBus (DBusUInt32 v) = show v
showDBus (DBusInt64 v) = show v
showDBus (DBusUInt64 v) = show v
showDBus (DBusDouble v) = show v
showDBus (DBusString (PackedString p)) = UTF8.toString p
showDBus (DBusObjectPath p) = unObjectPath p
showDBus (DBusSignature s) = show s
showDBus (DBusArray (SigDict _ _) kvs) = "{\n" ++ (unlines . map ("  "++) . map showDBus $ kvs) ++ "}"
showDBus (DBusArray _ xs) = intercalate "\n" . map showDBus $ xs
showDBus (DBusByteArray b) = show b
showDBus (DBusStruct _ xs) = intercalate "\n" . map showDBus $ xs
showDBus (DBusDict k v) = printf "%-30s = %s" ("\"" ++ showDBus k ++ "\"") (showDBus v)
showDBus (DBusVariant v) = showDBus v
showDBus (DBusUnixFD v) = show v
