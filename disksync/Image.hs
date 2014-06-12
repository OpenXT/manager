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

module Image where

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
import Tools.File ( doesFileExist )

import Types
import Db
import Rpc
import DiskSync
import ObjectPaths

instance DbRepr ImageId where
    toDbTree v = Leaf (show v)
    fromDbTree (Leaf str) = Just $ fromString str
    fromDbTree _ = Nothing

instance DbRepr ImageUuid where
    toDbTree v = Leaf (show v)
    fromDbTree (Leaf str) = Just $ fromString str
    fromDbTree _ = Nothing

instance DbRepr ImageState where
    toDbTree (ImageFailed x) = Record [("failed", Leaf (stringOfReason x))]
    toDbTree v = Leaf (f v) where
                          f ImageStopped = "stopped"
                          f ImageInProgress = "inprogress"
                          f ImageNetworkStalled = "network-stalled"
                          f ImageAdmCtlStalled = "admctl-stalled"
                          f ImagePaused = "paused"
                          f ImageFinished = "finished"
                          f ImageSrvCksumInProgress = "server-checksum-in-progress"
                          f x = error ("unhandled image state " ++ show x)

    fromDbTree (Record [("failed", Leaf v)]) = Just (ImageFailed $ reasonOfString v)
    fromDbTree (Leaf str) = conv str where
                            conv "stopped" = Just ImageStopped
                            conv "inprogress" = Just ImageInProgress
                            conv "network-stalled" = Just ImageNetworkStalled
                            conv "admctl-stalled" = Just ImageAdmCtlStalled
                            conv "paused" = Just ImagePaused
                            conv "finished" = Just ImageFinished
                            conv "server-checksum-in-progress" = Just ImageSrvCksumInProgress
                            conv _ = Nothing
    fromDbTree _ = Nothing

instance DbRepr Image where
    fromDbTree t =
        do image_uuid <- get "image-uuid"
           image_ordinal <- get "ordinal"
           disk_id <- get "disk-id"
           image_url <- get "image-url"
           transfer_url <- get "transfer-url"
           server_cert <- get "server-cert-path"
           client_cert <- get "client-cert-path"
           client_key <- get "client-key-path"
           encrypt_local <- get "encrypt-local-image"
           transfer_ctx <- get "transfer-context-id"
           return $ Image { imageUuid = fromString image_uuid
                          , imageOrdinal = read image_ordinal
                          , imageDiskId = fromString disk_id
                          , imageUrl = image_url
                          , imageTransferUrl = transfer_url
                          , imageCrypto = CryptoSpec {
                                            cryptoServerCertPath = server_cert
                                          , cryptoClientCertPath = client_cert
                                          , cryptoClientKeyPath = client_key
                                          }
                          , imageEncryptLocalImage = boolOfString encrypt_local
                          , imageTransferCtxId = fromString transfer_ctx
                          }
        where
          m = dbTreeToStringMap t
          get k = k `Map.lookup` m

    toDbTree t = Record [
                       ( "image-uuid", Leaf (show $ imageUuid t))
                     , ( "ordinal", Leaf (show $ imageOrdinal t))
                     , ( "disk-id", Leaf (show $ imageDiskId t))
                     , ( "image-url", Leaf (imageUrl t))
                     , ( "transfer-url", Leaf (imageTransferUrl t))
                     , ( "server-cert-path", Leaf (cryptoServerCertPath . imageCrypto $ t))
                     , ( "client-cert-path", Leaf (cryptoClientCertPath . imageCrypto $ t))
                     , ( "client-key-path", Leaf (cryptoClientKeyPath . imageCrypto $ t))
                     , ( "encrypt-local-image", Leaf (show $ imageEncryptLocalImage t))
                     , ( "transfer-context-id", Leaf ctx_id_str)
                     ]
                 where TransferCtxID ctx_id_str = imageTransferCtxId t

imageSetState :: ImageId -> ImageState -> DiskSync ()
imageSetState image_id state = liftRpc $ do
                                 dbWrite ( imageDBPath image_id ++ "/state" ) state
                                 info $ "changed image " ++ show image_id ++ " state: " ++ show state

imageGetState :: ImageId -> DiskSync ImageState
imageGetState image_id = liftRpc $ dbRead ( imageDBPath image_id ++ "/state" )

imageGetImage :: ImageId -> DiskSync ImageUuid
imageGetImage image_id = fromString <$> liftRpc (dbRead ( imageDBPath image_id ++ "/image-uuid" ))

imageGetDisk :: ImageId -> DiskSync DiskId
imageGetDisk image_id = fromString <$> liftRpc (dbRead ( imageDBPath image_id ++ "/disk-id" ))

imageGetOrdinal :: ImageId -> DiskSync Int
imageGetOrdinal image_id = liftRpc (dbReadWithDefault ( imageDBPath image_id ++ "/ordinal" ) 0)

imageList :: DiskSync [ImageId]
imageList =
    do strs <- liftRpc $ dbList imagesDBPath
       return . map fromString $ strs

imageRead :: ImageId -> DiskSync Image
imageRead image_id = liftRpc $ dbRead ( imageDBPath image_id )

imageCreate :: DiskId
            -> Int
            -> ImageUuid
            -> String
            -> String
            -> FilePath
            -> FilePath
            -> FilePath
            -> Bool
            -> String
            -> DiskSync ImageId
imageCreate disk_id image_ordinal image_uuid image_url transfer_url server_cert_path client_cert_path client_key_path encrypt_local transfer_ctx_id =
    do let image_id = imageId image
       existential_query image_id
       liftRpc $ dbWrite (imageDBPath image_id) image
       imageSetState image_id ImageStopped
       resetLocalFailureCount image_id 0
       liftIO . info $ "created image " ++ show image_id
       return image_id
    where
      image = Image {
                imageUuid = image_uuid
              , imageDiskId = disk_id
              , imageOrdinal = image_ordinal
              , imageUrl = image_url
              , imageTransferUrl = transfer_url
              , imageTransferCtxId = fromString transfer_ctx_id
              , imageCrypto = CryptoSpec {
                                cryptoServerCertPath = server_cert_path
                              , cryptoClientCertPath = client_cert_path
                              , cryptoClientKeyPath = client_key_path }
              , imageEncryptLocalImage = encrypt_local
              }
      existential_query image_id =
          do images <- imageList
             when (image_id `elem` images) $ error ("image " ++ show image_id ++ " already exists")

imageDelete :: ImageId -> DiskSync ()
imageDelete image_id =
    liftRpc $ do
      dbRm ( imageDBPath image_id )
      info $ "removed image " ++ show image_id

-- XXX: the following are routines for checking the state of image files on disk
--      these can be removed when the VHD management daemon is implemented
imageLocalPath :: ImageUuid -> FilePath
imageLocalPath image_uuid =
    "/storage/disks/" ++ show image_uuid ++ ".vhd"

-- check if a single image exists on disk
doesImageExist :: ImageUuid -> DiskSync Bool
doesImageExist image_uuid =
    liftIO $ doesFileExist (imageLocalPath image_uuid)

-- check if the logical contents of an image exist on disk.
-- this checks so see if there is a path from the specified image to the
-- root of the VHD chain that this image is a part of
imageInstalled :: ImageUuid -> DiskSync Bool
imageInstalled image_uuid =
    installed (imageLocalPath image_uuid)
    where
      installed path = do
        maybe_parent <- liftIO $ vhdParent path
        case maybe_parent of
          Nothing -> return False
          Just parent -> if is_root (chomp parent) then return True
                                                   else installed (chomp parent)
      is_root = endsWith "has no parent"

vhdParent :: FilePath -> IO (Maybe FilePath)
vhdParent path =
    spawnShell' ("vhd-util query -p -n " ++ path)

resetLocalFailureCount :: ImageId -> Int -> DiskSync ()
resetLocalFailureCount image_id p =
       liftRpc $ dbWrite (imageDBPath image_id ++ "/failure-count-local") p

getLocalFailureCount :: ImageId -> DiskSync Int
getLocalFailureCount image_id = liftRpc $ dbReadWithDefault ( imageDBPath image_id ++ "/failure-count-local" ) 0

incLocalFailureCount :: ImageId -> DiskSync ()
incLocalFailureCount image_id =
    do c <- getLocalFailureCount image_id
       liftRpc $ dbWrite (imageDBPath image_id ++ "/failure-count-local") (c+1)

rmLocalImage :: ImageUuid -> DiskSync (Maybe String)
rmLocalImage image_uuid =
    liftIO $ do spawnShell' ("rm -f \"" ++ imageLocalPath image_uuid ++ "\"")
                spawnShell' ("rm -f \"" ++ imageLocalPath image_uuid ++ ".sync.gen\"")
                spawnShell' ("rm -f \"" ++ imageLocalPath image_uuid ++ ".sync.meta\"")
                spawnShell' ("rm -f \"" ++ imageLocalPath image_uuid ++ ".sync.remote.cksum\"")
