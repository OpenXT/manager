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

module Config where

import Db
import Rpc
import ObjectPaths

---------------------------------
-- global disksync config options
---------------------------------

setEnableDvdCache :: Bool -> Rpc ()
setEnableDvdCache = dbWrite (diskSyncDBPath ++ "/enable-dvd-cache")

getEnableDvdCache :: Rpc (Bool)
getEnableDvdCache = dbReadWithDefault (diskSyncDBPath ++ "/enable-dvd-cache") True

getEnableForceLocalCheckSum :: Rpc (Bool)
getEnableForceLocalCheckSum = dbReadWithDefault (diskSyncDBPath ++ "/force-local-checksum") False

setEnableForceLocalCheckSum :: Bool -> Rpc ()
setEnableForceLocalCheckSum = dbWrite (diskSyncDBPath ++ "/force-local-checksum")

setEnableUsbCache :: Bool -> Rpc ()
setEnableUsbCache = dbWrite (diskSyncDBPath ++ "/enable-usb-cache")

getEnableUsbCache :: Rpc (Bool)
getEnableUsbCache = dbReadWithDefault (diskSyncDBPath ++ "/enable-usb-cache") True

setEnableUploadCompaction :: Bool -> Rpc ()
setEnableUploadCompaction = dbWrite (diskSyncDBPath ++ "/enable-upload-compaction")

getEnableUploadCompaction :: Rpc (Bool)
getEnableUploadCompaction = dbReadWithDefault (diskSyncDBPath ++ "/enable-upload-compaction") True

setDownloadCksize :: Int -> Rpc ()
setDownloadCksize = dbWrite (diskSyncDBPath ++ "/download-cksize")

getDownloadCksize :: Rpc (Int)
getDownloadCksize = dbReadWithDefault (diskSyncDBPath ++ "/download-cksize") 80 --MB

setUploadCksize :: Int -> Rpc ()
setUploadCksize = dbWrite (diskSyncDBPath ++ "/upload-cksize")

getUploadCksize :: Rpc (Int)
getUploadCksize = dbReadWithDefault (diskSyncDBPath ++ "/upload-cksize") 40 --MB

setVhdsyncLogLevel :: Int -> Rpc ()
setVhdsyncLogLevel = dbWrite (diskSyncDBPath ++ "/vhdsync-log-level")

getVhdsyncLogLevel :: Rpc (Int)
getVhdsyncLogLevel = dbReadWithDefault (diskSyncDBPath ++ "/vhdsync-log-level") 6

setVhdsyncVerbose :: Bool -> Rpc ()
setVhdsyncVerbose = dbWrite (diskSyncDBPath ++ "/vhdsync-verbose")

getVhdsyncVerbose :: Rpc (Bool)
getVhdsyncVerbose = dbReadWithDefault (diskSyncDBPath ++ "/vhdsync-verbose") False

setXferConnectTimeout :: Int -> Rpc ()
setXferConnectTimeout = dbWrite (diskSyncDBPath ++ "/xfer-connect-timeout")

getXferConnectTimeout :: Rpc (Int)
getXferConnectTimeout = dbReadWithDefault (diskSyncDBPath ++ "/xfer-connect-timeout") 10 --sec

setXferLowspeedLimit :: Int -> Rpc ()
setXferLowspeedLimit = dbWrite (diskSyncDBPath ++ "/xfer-lowspeed-limit")

getXferLowspeedLimit :: Rpc (Int)
getXferLowspeedLimit = dbReadWithDefault (diskSyncDBPath ++ "/xfer-lowspeed-limit") 1024 --Bps

setXferLowspeedTime :: Int -> Rpc ()
setXferLowspeedTime = dbWrite (diskSyncDBPath ++ "/xfer-lowspeed-time")

getXferLowspeedTime :: Rpc (Int)
getXferLowspeedTime = dbReadWithDefault (diskSyncDBPath ++ "/xfer-lowspeed-time") 5 --sec

setMaxParallelTasks :: Int -> Rpc ()
setMaxParallelTasks = dbWrite (diskSyncDBPath ++ "/max-parallel-tasks")

getMaxParallelTasks :: Rpc (Int)
getMaxParallelTasks = dbReadWithDefault (diskSyncDBPath ++ "/max-parallel-tasks") 1
