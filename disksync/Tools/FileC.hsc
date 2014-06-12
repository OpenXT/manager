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

{-# LANGUAGE CPP,ForeignFunctionInterface #-}
module Tools.FileC (
                     getTotalBlocks
                   , getAvailBlocks
                   , getBlockSize
                   ) where

import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Int
import Data.String
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

#include <unistd.h>
#include <sys/vfs.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data StatFs = StatFs {
      statType :: Int32
    , statBsize :: Int32
    , statBlocks :: Int64
    , statBfree :: Int64
    , statBavail :: Int64
    , statFiles :: Int64
    , statFfree :: Int64
    , statFsid :: Int32
    , statNamelen :: Int32
    }

instance Storable StatFs where
    alignment _ = #{alignment struct statfs}
    sizeOf    _ = #{size struct statfs}
    peek ptr    =
        do typ     <- #{peek struct statfs, f_type} ptr
           bsize   <- #{peek struct statfs, f_bsize} ptr
           blocks  <- #{peek struct statfs, f_blocks} ptr
           bfree   <- #{peek struct statfs, f_bfree} ptr
           bavail  <- #{peek struct statfs, f_bavail} ptr
           bfiles  <- #{peek struct statfs, f_files} ptr
           ffree   <- #{peek struct statfs, f_ffree} ptr
           fsid    <- #{peek struct statfs, f_fsid} ptr
           namelen <- #{peek struct statfs, f_namelen} ptr

           return StatFs { statType = typ
                         , statBsize = bsize
                         , statBlocks = blocks
                         , statBfree = bfree
                         , statBavail = bavail
                         , statFiles = bfiles
                         , statFfree = ffree
                         , statFsid = fsid
                         , statNamelen = namelen
                         }
    poke ptr s =
        do #{poke struct statfs, f_type} ptr (statType s)
           #{poke struct statfs, f_bsize} ptr (statBsize s)
           #{poke struct statfs, f_blocks} ptr (statBlocks s)
           #{poke struct statfs, f_bfree} ptr (statBfree s)
           #{poke struct statfs, f_bavail} ptr (statBavail s)
           #{poke struct statfs, f_files} ptr (statFiles s)
           #{poke struct statfs, f_ffree} ptr (statFfree s)
           #{poke struct statfs, f_fsid} ptr (statFsid s)
           #{poke struct statfs, f_namelen} ptr (statNamelen s)

foreign import ccall "sys/vfs.h statfs64" _statfs64 :: CString -> Ptr StatFs -> IO CInt

getTotalBlocks :: String -> IO Int64
getTotalBlocks path =
    B.useAsCString (UTF8.fromString path) $ \pathStr ->
        alloca $ \struct ->
            do rv <- _statfs64 pathStr struct
               fs <- peek struct
               return $ statBlocks fs

getAvailBlocks :: String -> IO Int64
getAvailBlocks path =
    B.useAsCString (UTF8.fromString path) $ \pathStr ->
        alloca $ \struct ->
            do rv <- _statfs64 pathStr struct
               fs <- peek struct
               return $ statBfree fs

getBlockSize :: String -> IO Int32
getBlockSize path =
    B.useAsCString (UTF8.fromString path) $ \pathStr ->
        alloca $ \struct ->
            do rv <- _statfs64 pathStr struct
               fs <- peek struct
               return $ statBsize fs


