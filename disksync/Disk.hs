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

{-# LANGUAGE TupleSections #-}
module Disk where

import Data.String
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent
import Control.Monad

import Tools.Log
import Tools.Misc

import Types
import Db
import Rpc
import Config
import DiskSync
import Image
import ObjectPaths
import VhdSync
import Errors

instance DbRepr DiskId where
    toDbTree v = Leaf (show v)
    fromDbTree (Leaf str) = Just $ fromString str
    fromDbTree _ = Nothing

instance DbRepr DiskUuid where
    toDbTree v = Leaf (show v)
    fromDbTree (Leaf str) = Just $ fromString str
    fromDbTree _ = Nothing

instance DbRepr DiskState where
    toDbTree v = Leaf (diskStateStr v)

    fromDbTree (Leaf s) = f s
        where
          f "stopped" = Just DiskStopped
          f "inprogress" = Just DiskInProgress
          f "network-stalled" = Just DiskNetworkStalled
          f "admctl-stalled" = Just DiskAdmCtlStalled
          f "paused" = Just DiskPaused
          f "finished" = Just DiskFinished
          f "failed" = Just DiskFinished
          f "server-checksum-in-progress" = Just DiskSrvCksumInProgress
          f _ = Nothing
    fromDbTree _ = Nothing

instance DbRepr Disk where
    fromDbTree t =
        do disk_uuid <- get "disk-uuid"
           group_id <- get "group-id"
           dir <- get "direction"
           return $ Disk { diskUuid = fromString disk_uuid
                         , diskGroupId = fromString group_id
                         , diskDirection = dirOfString dir
                         }
        where
          m = dbTreeToStringMap t
          get k = k `Map.lookup` m

    toDbTree t = Record [
                       ( "disk-uuid", Leaf (show $ diskUuid t))
                     , ( "group-id", Leaf (show $ diskGroupId t))
                     , ( "direction", Leaf (stringOfDir $ diskDirection t))
                     ]

diskGetState :: DiskId -> DiskSync DiskState
diskGetState disk_id =
    do images <- diskGetImages disk_id
       image_states <- mapM imageGetState images
       return . evalDiskState $ image_states

-- determine disk state from individual image states
evalDiskState :: [ ImageState ] -> DiskState
evalDiskState image_states
    | all (== ImageFinished) image_states = DiskFinished
    | any failed image_states = DiskFailed
    | any (== ImageInProgress) image_states = DiskInProgress
    | any (== ImageNetworkStalled) image_states = DiskNetworkStalled
    | any (== ImageAdmCtlStalled) image_states = DiskAdmCtlStalled
    | any (== ImageSrvCksumInProgress) image_states = DiskSrvCksumInProgress
    | any (== ImagePaused) image_states = DiskPaused
    | any (== ImageStopped) image_states = DiskStopped
    | null image_states = DiskStopped
    | otherwise = error "could not evaluate disk state"
    where
      failed (ImageFailed _) = True
      failed _               = False

diskGetDisk :: DiskId -> DiskSync DiskUuid
diskGetDisk disk_id = fromString <$> liftRpc (dbRead ( diskDBPath disk_id ++ "/disk-uuid" ))

diskGetGroup :: DiskId -> DiskSync GroupId
diskGetGroup disk_id = fromString <$> liftRpc (dbRead ( diskDBPath disk_id ++ "/group-id" ))

diskList :: DiskSync [DiskId]
diskList =
    do strs <- liftRpc $ dbList disksDBPath
       return . map fromString $ strs

diskRead :: DiskId -> DiskSync Disk
diskRead disk_id = liftRpc $ dbRead ( diskDBPath disk_id )

diskCreate :: GroupId -> DiskUuid -> Direction -> DiskSync DiskId
diskCreate group_id disk_uuid dir =
    do let disk_id = diskId disk
       existential_query disk_id
       liftRpc $ dbWrite (diskDBPath disk_id) disk
       liftIO . info $ "created disk " ++ show disk_id
       return disk_id
    where
      disk = Disk {
                 diskUuid = disk_uuid
               , diskGroupId = group_id
               , diskDirection = dir
               }
      existential_query disk_id =
          do disks <- diskList
             when (disk_id `elem` disks) $ error ("disk " ++ show disk_id ++ " already exists")

diskDelete :: DiskId -> DiskSync ()
diskDelete disk_id =
    do mapM imageDelete =<< diskGetImages disk_id
       liftRpc $ do
         dbRm (diskDBPath disk_id)
         info $ "removed disk " ++ show disk_id

diskGetImages :: DiskId -> DiskSync [ImageId]
diskGetImages disk_id =
    do images <- filterM (member disk_id) =<< imageList
       ordinals <- mapM imageGetOrdinal images
       return (ordered_images images ordinals)
    where
      member disk_id image_id =
          imageGetDisk image_id >>= return . (==) disk_id

      ordered_images images ordinals =
          snd $ unzip $ sortBy ord $ zip ordinals images
          where
            ord :: (Ord a) => (a,b) -> (a,b) -> Ordering
            ord (x,_) (y,_) = compare x y

diskGetNextImage :: DiskId -> ImageId -> DiskSync (Maybe ImageId)
diskGetNextImage disk_id image_id =
    do images <- diskGetImages disk_id
       let rest = drop 1 $ dropWhile (/= image_id) images
       return (next rest)
    where
      next []     = Nothing
      next (x:xs) = Just x

diskStart :: DisableXferCache -> DiskId -> DiskSync (Maybe ActiveTask)
diskStart maybe_disable_cache disk_id = case_ =<< findActiveTask disk_id
  where case_ Nothing = do
          image_id <- head <$> diskGetImages disk_id
          disk <- diskRead disk_id
          image <- imageRead image_id
          when ( diskDirection disk == Up ) $ do
            exists <- doesImageExist $ imageUuid image
            unless exists $ failUploadDiskDoesNotExist (diskId disk)
          diskMaybeStartImage maybe_disable_cache disk image
        case_ current = do
          warn $ "disk " ++ show disk_id ++ " already in progress, not restarting"
          return current

diskMaybeStartImage :: DisableXferCache -> Disk -> Image -> DiskSync (Maybe ActiveTask)
diskMaybeStartImage maybe_disable_cache disk image = case_ $ diskDirection disk
    where
      disk_id = diskId disk
      start_xfer = diskStartImage maybe_disable_cache disk image

      case_ Up   = return . Just =<< start_xfer Up
      case_ Down = do
        installed <- imageInstalled $ imageUuid image
        if installed
           then do
             info $ "disk " ++ show disk_id ++ " already installed, not downloading"
             diskFinished disk_id
             return Nothing
           else do
             return . Just =<< start_xfer Down

diskFinished :: DiskId -> DiskSync ()
diskFinished disk_id =
    mapM_ (flip imageSetState $ ImageFinished) =<< diskGetImages disk_id

diskStartImage :: DisableXferCache -> Disk -> Image -> Direction -> DiskSync ActiveTask
diskStartImage maybe_disable_cache disk image direction =
    do liftRpc $ make_options_xfer_cache maybe_disable_cache
       opts <- liftRpc $ make_options direction (imageEncryptLocalImage image)
       handle <- liftIO $ call direction opts (imageLocalPath $ imageUuid image) (imageTransferUrl image) (imageUrl image) (imageCrypto image) (imageTransferCtxId image)
       imageSetState image_id ImageInProgress
       let active_task = newActiveTask image disk handle
       addActiveTask active_task
       liftIO . info $ "started task " ++ show image_id
       return active_task
    where
      image_id = imageId image

      call Up   = startUpload
      call Down = startDownload

      make_options Up _ = do compact <- getEnableUploadCompaction
                             options <- make_common_options =<< getUploadCksize
                             return $ UploadOptions {enableCompaction = compact,
                                                     commonOptions = options}
      make_options Down encrypted = do dvdcache <- getEnableDvdCache
                                       usbcache <- getEnableUsbCache
                                       forcelocalchecksum <- getEnableForceLocalCheckSum
                                       options <- make_common_options =<< getDownloadCksize
                                       return $ DownloadOptions {enableDvdCache = dvdcache,
                                                                 enableUsbCache = usbcache,
                                                                 forceLocalCheckSum = forcelocalchecksum,
                                                                 encryptLocalImage = encrypted,
                                                                 commonOptions = options}

      make_common_options cksize = do loglevel <- getVhdsyncLogLevel
                                      verbose <- getVhdsyncVerbose
                                      lowlimit <- getXferLowspeedLimit
                                      lowtime <- getXferLowspeedTime
                                      ctimeout <- getXferConnectTimeout
                                      let mb_to_kb = (*) 1024
                                      return $ CommonTransferOptions {chunkSize = mb_to_kb cksize,
                                                                      logLevel = loglevel,
                                                                      verbose = verbose,
                                                                      lowspeedLimit = lowlimit,
                                                                      lowspeedTime = lowtime,
                                                                      connectTimeout = ctimeout}

      make_options_xfer_cache DisableDvdCache = do setEnableDvdCache False
						   setEnableUsbCache False -- Since USB cache has priority

      make_options_xfer_cache DisableUsbCache = do setEnableUsbCache False

      make_options_xfer_cache NoDisable = return()

onActiveTask :: (ActiveTask -> DiskSync ()) -> DiskId -> DiskSync ()
onActiveTask f disk_id =
    findActiveTask disk_id >>= \t ->
        case t of
          Nothing -> return ()
          Just active_task -> f active_task

diskStop :: DiskId -> DiskSync ()
diskStop disk_id =
    onActiveTask stop disk_id
    where
      stop active_task =
          do liftIO $ do
               info $ "stopping task " ++ show disk_id ++ "..."
               cleanupSyncProcess (atSync active_task)
             rmActiveTask disk_id
             imageSetState image_id ImageStopped
             info $ "DONE stopping task " ++ show disk_id
          where
            image_id = imageId $ atImage active_task

diskPause :: DiskId -> DiskSync ()
diskPause disk_id =
    onActiveTask pause disk_id
    where
      pause active_task =
          do liftIO $ do
               info $ "pausing task " ++ show disk_id ++ "..."
               pauseSyncProcess (atSync active_task)
             info $ "DONE pausing task " ++ show disk_id

diskResume :: DiskId -> DiskSync ()
diskResume disk_id =
    onActiveTask resume disk_id
    where
      resume active_task =
          do liftIO $ do
               info $ "resuming task " ++ show disk_id ++ "..."
               resumeSyncProcess (atSync active_task)
             info $ "DONE resuming task " ++ show disk_id

diskGetFailureCount :: DiskId -> DiskSync Int
diskGetFailureCount disk_id = liftRpc $ dbReadWithDefault ( diskDBPath disk_id ++ "/failure-count" ) 0

diskIncFailureCount :: DiskId -> DiskSync ()
diskIncFailureCount disk_id =
    do c <- diskGetFailureCount disk_id
       liftRpc $ dbWrite (diskDBPath disk_id ++ "/failure-count") (c+1)

-----------------
-- queues
-----------------

inactiveState DiskFinished = True
inactiveState DiskStopped  = True
inactiveState DiskFailed   = True
inactiveState _            = False

taskGetQueuePosition :: DiskId -> DiskSync (Maybe Int)
taskGetQueuePosition disk_id = liftRpc $ dbMaybeRead (diskDBPath disk_id ++ "/queue-pos")

taskSetQueuePosition :: DiskId -> Int -> DiskSync ()
taskSetQueuePosition disk_id p =
    liftRpc $ dbWrite (diskDBPath disk_id ++ "/queue-pos") p

taskQueueContents :: DiskSync [DiskId]
taskQueueContents =
    liftM (fmap snd . sortBy (comparing fst) . catMaybes) .
    mapM tag =<< diskList
  where
    -- tag diskId with queue position for sorting
    tag :: DiskId -> DiskSync (Maybe (Int, DiskId))
    tag disk_id = liftM (fmap (,disk_id)) $ taskGetQueuePosition disk_id


taskEnqueue :: DiskId -> DiskSync ()
taskEnqueue disk_id =
    do my_qp <- taskGetQueuePosition disk_id
       when (isNothing my_qp) $ do
         tasks <- filter (/= disk_id) <$> diskList
         max_qp <- maximum_ . catMaybes <$> mapM taskGetQueuePosition tasks
         taskSetQueuePosition disk_id (max_qp + 1)
    where
      maximum_ [] = 0
      maximum_ xs = maximum xs

taskDequeue :: DiskId -> DiskSync ()
taskDequeue disk_id = do
    liftRpc $ dbRm (diskDBPath disk_id ++ "/queue-pos")

taskDequeueInactive :: DiskId -> DiskSync Bool
taskDequeueInactive disk_id =
    do deq <- inactiveState <$> diskGetState disk_id
       when deq $ taskDequeue disk_id
       return deq
