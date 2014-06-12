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

module DiskGroup where

import Data.String
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad
import Control.Concurrent
import Control.Applicative
import Control.Arrow
import qualified Data.Traversable

import Tools.Log
import Tools.Misc

import Types
import ObjectPaths
import DiskSync
import Db
import Disk
import Image
import VhdSync
import Config
import qualified ImageRpc

import Rpc
import Rpc.Autogen.DisksyncmgrNotify

instance DbRepr GroupId where
    toDbTree uuid = Leaf (show uuid)
    fromDbTree (Leaf str) = Just $ fromString str
    fromDbTree _ = Nothing

instance DbRepr GroupState where
    toDbTree s = Leaf (groupStateStr s)

    fromDbTree (Leaf str) = conv str
        where
          conv "stopped" = Just GroupStopped
          conv "inprogress" = Just GroupInProgress
          conv "network-stalled" = Just GroupNetworkStalled
          conv "admctl-stalled" = Just GroupAdmCtlStalled
          conv "paused" = Just GroupPaused
          conv "finished" = Just GroupFinished
          conv "failed" = Just GroupFailed
          conv "server-checksum-in-progress" = Just GroupSrvCksumInProgress
          conv _ = Nothing
    fromDbTree _ = Nothing

groupList :: DiskSync [GroupId]
groupList = liftRpc $
    map fromString <$> dbList diskGroupsDBPath

groupCreate :: DiskSync GroupId
groupCreate = do
    group_id <- liftIO $ uuidGen
    existential_query group_id
    liftRpc $ dbWrite (diskGroupDBPath group_id ++ "/id") (show group_id)
    return group_id
    where
      existential_query group_id =
          do groups <- groupList
             when (group_id `elem` groups) $ error ("group " ++ show group_id ++ " already exists")

groupGetDisks :: GroupId -> DiskSync [DiskId]
groupGetDisks group_id =
  filterM (member group_id) =<< diskList
    where
      member group_id disk_id =
        (==) group_id <$> diskGetGroup disk_id

groupStart :: GroupId -> DiskSync ()
groupStart group_id =
    do mapM_ taskEnqueue =<< groupGetDisks group_id
       wakeTaskQueueDefault NoDisable
       sendGroupStateChange' group_id

groupStartAndMonitorTask :: DisableXferCache -> GroupId -> DiskId -> DiskSync ()
groupStartAndMonitorTask  maybe_disable_cache group_id disk_id =
    do at <- diskStart maybe_disable_cache disk_id
       when (isNothing at) $ do taskDequeueInactive disk_id
                                wakeTaskQueueDefault maybe_disable_cache
       groupMonitorTask group_id at

groupMonitorTask :: GroupId -> Maybe ActiveTask -> DiskSync ()
groupMonitorTask group_id Nothing   = sendGroupStateChange' group_id
groupMonitorTask group_id (Just at) =
    do rpc_context <- liftRpc $ rpcGetContext
       app_context <- getAppContext
       liftIO . forkIO $ (rpc rpc_context $ monitor app_context at) >>= \status ->
           case status of
             Left err -> warn $ "error monitoring task: " ++ show err
             Right _ -> return ()
       sendGroupStateChange' group_id

groupStop :: GroupId -> DiskSync ()
groupStop group_id =
    do mapM_ diskStop =<< groupGetDisks group_id
       groupDequeueInactive group_id
       sendGroupStateChange' group_id

groupStopAndStartNext :: GroupId -> DiskSync ()
groupStopAndStartNext group_id =
    do mapM_ diskStop =<< groupGetDisks group_id
       groupDequeueInactive group_id
       wakeTaskQueueDefault NoDisable
       sendGroupStateChange' group_id

groupPause :: GroupId -> DiskSync ()
groupPause group_id =
    mapM_ diskPause =<< groupGetDisks group_id

groupResume :: GroupId -> DiskSync ()
groupResume group_id =
    do mapM_ (onActiveTask resume) =<< groupGetDisks group_id
    where
      resume active_task = do
          let disk_id = diskId $ atDisk active_task
          st <- diskGetState disk_id
          do_resume st disk_id
      do_resume DiskNetworkStalled disk_id = diskResume disk_id
      do_resume DiskPaused         disk_id = diskResume disk_id
      do_resume st disk_id =
          warn $ "called resume on disk " ++ show disk_id ++ " in state " ++ show st

groupDelete :: GroupId -> DiskSync ()
groupDelete group_id =
    do mapM diskDelete =<< groupGetDisks group_id
       groupDequeue group_id
       wakeTaskQueueDefault NoDisable
       liftRpc $ do
         dbRm (diskGroupDBPath group_id)
         info $ "removed group " ++ show group_id

-- figure group state from individual disk states
evalGroupState :: [ DiskState ] -> GroupState
evalGroupState states
    | all (== DiskFinished) states = GroupFinished
    | any failed states = GroupFailed
    | any (== DiskInProgress) states = GroupInProgress
    | any (== DiskNetworkStalled) states = GroupNetworkStalled
    | any (== DiskAdmCtlStalled) states = GroupAdmCtlStalled
    | any (== DiskSrvCksumInProgress) states = GroupSrvCksumInProgress
    | any (== DiskPaused) states = GroupPaused
    | any (== DiskStopped) states = GroupStopped
    | null states = GroupStopped
    | otherwise = error "could not evaluate group state"
    where
      failed DiskFailed = True
      failed _          = False

groupGetState :: GroupId -> DiskSync GroupState
groupGetState =
    liftM evalGroupState .
    (mapM (diskGetState) <=< groupGetDisks)

maxRetryCount :: Int
maxRetryCount = 3

maybeRetryTask :: GroupId -> ActiveTask -> DiskSync Bool
maybeRetryTask group_id at =
    do cnt <- diskGetFailureCount disk_id
       if cnt <= maxRetryCount && retryable at
          then retryTask NoDisable group_id disk_id >> return True
          else return False
    where
      disk_id = diskId $ atDisk at

      retryable = retryable_result . atResult

      retryable_result Nothing  = True
      retryable_result (Just x) = retryable_reason (srReason x)

      retryable_reason InsufficientSpace = False --server does not have enough space
      retryable_reason AccessDenied      = False --invalid xfer context provided
      retryable_reason _                 = True

retryTask :: DisableXferCache -> GroupId -> DiskId -> DiskSync ()
retryTask maybe_disable_cache group_id disk_id =
    do info $ "RETRY failed task " ++ show disk_id
       diskStop disk_id
       groupStartAndMonitorTask maybe_disable_cache group_id disk_id

monitor :: AppContext -> ActiveTask -> Rpc ()
monitor ctx active_task =
    liftIO (waitSyncEvent $ atSync active_task) >>= process

    where
      image_id = imageId $ atImage active_task
      image_uuid = imageUuid $ atImage active_task
      disk = atDisk $ active_task
      disk_id = diskId disk
      group_id = diskGroupId disk

      image_delete img_id =
          do imageDelete img_id
             liftRpc $ ImageRpc.unexpose img_id

      process Finished =
          do info $ "finished image " ++ show image_id
             runDiskSync ctx $ final_state ImageFinished

      process OnDemandStop =
          info $ "image " ++ show image_id ++ " stopped on demand"

      process (Failed err) =
          do warn ("error processing image " ++ show image_id ++ ": " ++ show err)
             runDiskSync ctx $ case_ (atResult active_task)
          where
            case_ :: Maybe SyncResultDetails -> DiskSync ()
            case_ Nothing   = maybe_retry Success --success will get xlat'd into ImageFailUnknown
            case_ (Just sr) = handle_ (diskDirection disk) (srReason sr)

            handle_ :: Direction -> SyncResultReason -> DiskSync ()
            handle_ Down InvalidGenNumber   = do info $ "invalid gen number during download " ++ show image_id
                                                 retry NoDisable
            handle_ Down ImageCoalesced     = do info $ "image coalesced on server during download " ++ show image_id
                                                 ordinal <- imageGetOrdinal image_id
                                                 if (ordinal == 1)
                                                    then sendGroupInterrupted group_id (diskUuid disk) image_uuid ImageCoalesced
                                                    else do
                                                       image_delete image_id
                                                       rmLocalImage image_uuid
                                                       retry NoDisable
            handle_ Down LocalChecksumFailureDvd = do info $ "Local checksum failed due to bad vhd on DVD media " ++ show image_id
                                                      retry DisableDvdCache
            handle_ Down LocalChecksumFailureUsb = do info $ "Local checksum failed due to bad vhd on USB media " ++ show image_id
						      retry DisableUsbCache
            handle_ Down LocalChecksumNotFoundDvd = do info $ "Local checksum not found on DVD media " ++ show image_id
                                                       retry DisableDvdCache
            handle_ Down LocalChecksumNotFoundUsb = do info $ "Local checksum not found on USB media " ++ show image_id
						       retry DisableUsbCache
            handle_ Down LocalChecksumPartFailDvd = do info $ "Checking for partial failure on DVD media " ++ show image_id
                                                       incLocalFailureCount image_id
                                                       cnt <- getLocalFailureCount image_id
                                                       if cnt > 2
                                                          then do
                                                           resetLocalFailureCount image_id 0
                                                           info $ "Stopped download from DVD due to bad vhd on DVD "
                                                           retry DisableDvdCache
                                                          else
                                                           retry NoDisable
            handle_ Down LocalChecksumPartFailUsb = do info $ "Checking for partial failure on USB media" ++ show image_id
                                                       incLocalFailureCount image_id
                                                       cnt <- getLocalFailureCount image_id
                                                       if cnt > 2
                                                          then do
                                                           resetLocalFailureCount image_id 0
                                                           info $ "Stopped download from USB due to bad vhd on USB "
                                                           retry DisableUsbCache
                                                          else
                                                           retry NoDisable
            handle_ Down LocalChecksumMalformedDvd = do info $ "Malformed Digest on DVD " ++ show image_id
                                                        retry  DisableDvdCache
            handle_ Down LocalChecksumMalformedUsb = do info $ "Malformed Digest on USB " ++ show image_id
						        retry DisableUsbCache
            handle_ Down x                  = maybe_retry x
            handle_ Up   ImageCoalesced     = do info $ "image coalesced on server during upload " ++ show image_id ++ "group_id" ++ show group_id
                                                 imageSetState image_id (ImageFailed ImageFailCoalesced)
                                                 sendGroupInterrupted group_id (diskUuid disk) image_uuid ImageCoalesced
                                                 groupStopAndStartNext group_id
            handle_ Up   ImageAlreadyExists = do info $ "image already existed on server during upload " ++ show image_id
                                                 diskFinished disk_id
                                                 taskDequeueInactive disk_id
                                                 wakeTaskQueueDefault NoDisable
            handle_ Up   x                  = maybe_retry x

            retry :: DisableXferCache -> DiskSync ()
	    retry maybe_disable_cache = do
              retryTask  maybe_disable_cache group_id disk_id
	      sendGroupStateChange' group_id

            maybe_retry :: SyncResultReason -> DiskSync ()
            maybe_retry reason = do
              diskIncFailureCount disk_id
              retried <- maybeRetryTask group_id active_task
              if retried
                 then sendGroupStateChange' group_id
                 else do
                    let r = fail_reason reason
                    when ( r == ImageFailUnknown ) $ warn ("sync result reason " ++ show reason ++ " mapped to ImageFailUnknown")
                    final_state $ ImageFailed (fail_reason reason)
                    groupStopAndStartNext group_id
              where
                --collapse SyncResultReason into ImageFailureReason
                fail_reason Success                   = ImageFailUnknown --task failed but the last msg received was success
                fail_reason FileAccessError           = ImageFailFileAccessError
                fail_reason InvalidChunkParams        = ImageFailInvalidRequest
                fail_reason InvalidRange              = ImageFailInvalidRequest
                fail_reason MalformedRangeHeader      = ImageFailInvalidRequest
                fail_reason MalformedURI              = ImageFailInvalidRequest
                fail_reason InvalidRpc                = ImageFailInvalidRequest
                fail_reason InvalidRpcMethod          = ImageFailInvalidRequest
                fail_reason TransferSizeTooLarge      = ImageFailInvalidRequest
                fail_reason DigestMalformed           = ImageFailInvalidRequest
                fail_reason FileNotUnique             = ImageFailFileNotUnique
                fail_reason FileNotExists             = ImageFailFileNotExists
                fail_reason FileNotTracked            = ImageFailFileNotTracked
                fail_reason InsufficientSpace         = ImageFailInsufficientSpace
                fail_reason InvalidImageState         = ImageFailInvalidImageState
                fail_reason DigestMismatch            = ImageFailChecksumMismatch
                fail_reason IncompatibleVersion       = ImageFailIncompatibleVersion
                fail_reason InvalidKey                = ImageFailInvalidKey
                fail_reason AccessDenied              = ImageFailAccessDenied
                fail_reason HTTPNotAllowed            = ImageFailHTTPNotAllowed
                fail_reason TooManyXfers              = ImageFailTooManyXfers
                fail_reason AdmissionError            = ImageFailInternalServerError
                fail_reason ImageCoalesced            = ImageFailCoalesced
                fail_reason LocalChecksumFailureDvd   = ImageFailLocalChecksumFailureDvd
                fail_reason LocalChecksumFailureUsb   = ImageFailLocalChecksumFailureUsb
                fail_reason LocalChecksumNotFoundDvd  = ImageFailLocalChecksumNotFoundDvd
                fail_reason LocalChecksumNotFoundUsb  = ImageFailLocalChecksumNotFoundUsb
                fail_reason LocalChecksumPartFailDvd  = ImageFailLocalChecksumPartFailDvd
                fail_reason LocalChecksumPartFailUsb  = ImageFailLocalChecksumPartFailUsb
                fail_reason LocalChecksumMalformedDvd = ImageFailLocalChecksumMalformedDvd
                fail_reason LocalChecksumMalformedUsb = ImageFailLocalChecksumMalformedUsb
                fail_reason x                         = ImageFailUnknown

      process (StageProgress stage progress) =
          continue <=< runDiskSync ctx $ do
                  progressActiveTask disk_id stage progress
                  reportGroupProgress group_id
                  get_active_task

      process (StateChange state reason) =
          continue <=< runDiskSync ctx $ do
                  info $ "task " ++ show disk_id ++ " state changed to " ++ show state ++ " with reason " ++ show reason
                  state_change state reason
                  get_active_task

      process (Result details) =
          continue <=< runDiskSync ctx $ do
                  info $ "task " ++ show disk_id ++ " result code " ++ show code ++ " with reason " ++ show reason
                  resultActiveTask disk_id details
                  get_active_task
          where
            code = srCode details
            reason = srReason details

      process (Admctl details) =
          continue <=< runDiskSync ctx $ do
                  info $ "task " ++ show disk_id ++ " admctl time " ++ show time ++ " next_contact " ++ show next_contact ++ " qpos " ++ show qpos
                  admctlActiveTask disk_id details
                  get_active_task
          where
            time = saTime details
            next_contact = saNextContact details
            qpos = saQueuePosition details

      process (ServerCksum details) =
          continue <=< runDiskSync ctx $ do
                  info $ "task " ++ show disk_id ++ " server busy checksumming " ++ show time ++ " next_contact " ++ show next_contact
                  serverCksumActiveTask disk_id details
                  get_active_task
          where
            time = ckTime details
            next_contact = ckNextContact details

      -- message sent by vhd-sync.compress to flush the monitor fd between vhd-sync invocations; ignore
      process ClearThroat = continue active_task

      process (Unexpected cmd) = do
          warn ("unexpected command read (task " ++ show disk_id ++ "): " ++ show cmd)
          continue active_task

      get_active_task = fromJust <$> findActiveTask disk_id

      continue = monitor ctx

      state_change state reason =
          case next_state state reason of
            Nothing -> info $ "ignoring state change message"
            Just s -> do imageSetState image_id s
                         sendGroupStateChange' group_id
          where
            next_state Running _ = Just ImageInProgress
            next_state Paused User = Just ImagePaused
            next_state Paused NetworkInterruption = Just ImageNetworkStalled
            next_state Paused AdmissionControl = Just ImageAdmCtlStalled
            next_state Paused ServerChecksumming = Just ImageSrvCksumInProgress
            next_state Paused None = Nothing

      final_state st =
          do rmActiveTask disk_id
             imageSetState image_id st
             when ( st == ImageFinished ) $ start_next_image image_id
             taskDequeueInactive disk_id
             sendGroupStateChange' group_id
             wakeTaskQueueDefault NoDisable
          where
            direction = diskDirection disk
            start_next_image image_id = do
              maybe_next <- diskGetNextImage disk_id image_id
              when ( isJust maybe_next ) $ do
                                 image <- imageRead $ fromJust maybe_next
                                 exists <- doesImageExist $ imageUuid image
                                 if direction == Up && not exists
                                    then do
                                      --XC-5819: an intermediary image in the disk chain has been coalesced away.
                                      --         prune it from the list of images to upload
                                      image_delete (imageId image)
                                      start_next_image (imageId image)
                                    else
                                      (groupMonitorTask group_id =<< diskMaybeStartImage NoDisable disk image)

groupGetAdmctl :: GroupId -> DiskSync (Maybe (Int, Double))
groupGetAdmctl group_id =
  msum . map (fmap (saQueuePosition &&& saNextContact) . atAdmctl) .
  filter in_group <$> getActiveTasks
    where
      in_group at = group_id == (diskGroupId $ atDisk at)

-- evaluate task group progress based on set of inidvidual task progresses
evalGroupProgress :: [ (SyncStage, SyncProgress) ] -> Maybe (SyncStage, Double, Double, Double)
evalGroupProgress [] = Nothing
evalGroupProgress tasks
    | Transfer `elem` stages = Just (Transfer, accum Transfer `divide` total Transfer, ul, dl)
    | Chksum   `elem` stages = Just (Chksum, accum Chksum `divide` total Chksum, ul, dl)
    | Install  `elem` stages = Just (Install, accum Install `divide` total Install, ul, dl)
    | otherwise              = Nothing

    where
      (stages,_) = unzip tasks

      acc `divide` 0.0 = 0.0
      acc `divide` tot = acc / tot

      ul = sum . map (progressUlSpeed . snd) $ tasks
      dl = sum . map (progressDlSpeed . snd) $ tasks

      stage_progresses stage_type = map snd . filter (\(t,_) -> t == stage_type) $ tasks
      accum stage_type = sum . map (\p -> progressTotal p - progressRemaining p)  $ stage_progresses stage_type
      total stage_type = sum . map progressTotal $ stage_progresses stage_type

groupGetProgress :: GroupId -> DiskSync (Maybe (String, Double, Double, Double))
groupGetProgress group_id =
  fmap (fmap transform
        . evalGroupProgress . mapMaybe atProgress . filter in_group)
  getActiveTasks
    where
      transform (stage,completeness,ul,dl) =
        (stage_str stage, bound01 completeness, ul, dl)

      stage_str Transfer = "transfer"
      stage_str Chksum   = "checksum"
      stage_str Install  = "installation"

      bound01 = min 1 . max 0
      in_group at = group_id == (diskGroupId $ atDisk at)

void :: Monad m => m a -> m ()
void = (>> return ())

reportGroupProgress :: GroupId -> DiskSync ()
reportGroupProgress group_id = void $
    -- Only report if groupGetProgress reports: We can't estimate without data.
    -- Note: So here we use sequence with the following type:
    -- Maybe (DiskSync a) -> DiskSync (Maybe a)
    (Data.Traversable.sequence =<< fmap report <$> groupGetProgress group_id
    -- Let the compiler check that we are doing the Right Thing:
    :: DiskSync (Maybe ()))
  where report (stage,completeness,ul,dl) =
            liftRpc $ notifyComCitrixXenclientDisksyncmgrDiskGroupProgress
                (fromString "/")
                (show group_id)
                stage completeness
                ul dl
                (diskGroupObjPath group_id)

sendGroupStateChange :: GroupId -> GroupState -> DiskSync ()
sendGroupStateChange group_id state =
    liftRpc $
            notifyComCitrixXenclientDisksyncmgrDiskGroupStateChange
              (fromString "/")
              (show group_id)
              (groupStateStr state)
              (diskGroupObjPath group_id)

sendGroupStateChange' :: GroupId -> DiskSync ()
sendGroupStateChange' group_id =
    sendGroupStateChange group_id =<< groupGetState group_id

sendGroupInterrupted :: GroupId -> DiskUuid -> ImageUuid -> SyncResultReason -> DiskSync ()
sendGroupInterrupted group_id disk_uuid image_uuid reason=
    liftRpc $
            notifyComCitrixXenclientDisksyncmgrDiskGroupInterrupted
              (fromString "/")
              (show group_id)
              (show disk_uuid)
              (diskGroupObjPath group_id)
              (show image_uuid)
              (show reason)
-----------------
-- disk group queing
-----------------
groupDequeueInactive :: GroupId -> DiskSync ()
groupDequeueInactive group_id = mapM_ taskDequeueInactive =<< groupGetDisks group_id

groupDequeue :: GroupId -> DiskSync ()
groupDequeue group_id = mapM_ taskDequeue =<< groupGetDisks group_id

-- start first n inactive tasks in queue
wakeTaskQueue :: DisableXferCache -> Int -> DiskSync ()
wakeTaskQueue maybe_disable_cache n =
    do q <- taskQueueContents
       wake q 0
    where
      -- Smells like a foldrM_ but with short-circuiting.  Perhaps foldM_ over ContT does it?
      wake [] _ = return ()
      wake (t : ts) num_awake
          | num_awake >= n  = return ()
          | otherwise       =
              do inactive <- inactiveState <$> diskGetState t
                 when inactive $ do
                      group <- diskGetGroup t
                      info $ "starting queued task " ++ show t ++ " (group " ++ show group ++ ")"
                      groupStartAndMonitorTask maybe_disable_cache group t
                 wake ts (num_awake+1)

wakeTaskQueueDefault :: DisableXferCache -> DiskSync ()
wakeTaskQueueDefault maybe_disable_cache  = do wakeTaskQueue maybe_disable_cache =<< liftRpc getMaxParallelTasks
