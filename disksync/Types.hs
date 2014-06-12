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

module Types (
      Direction (Up, Down)
    , Image (..)
    , imageId
    , Disk (..)
    , diskId
    , CryptoSpec (..)
    , CommonTransferOptions (..)
    , TransferOptions (..)
    , TransferCtxID (..)
    , GroupId
    , DiskId
    , ImageId
    , DiskUuid
    , ImageUuid
    , DiskState (..)
    , ImageState (..)
    , ImageFailureReason (..)
    , GroupState (..)
    , DisableXferCache (..)
    , stringOfDir, dirOfString
    , boolOfString
    , stringOfReason, reasonOfString
    , diskStateStr
    , groupStateStr
    ) where


import Data.String
import List ( intersperse )

data Direction = Up | Down
              deriving (Eq,Show)

stringOfDir Up = "up"
stringOfDir Down = "down"

dirOfString "up" = Up
dirOfString _ = Down

boolOfString "True" = True
boolOfString _ = False


-- Unique Identifiers for Groups, Disks, and Images
-- All of these are globally unique and may be used as
-- keys for storing/retrieving these objects from the DB

data GroupId = GroupId String deriving Eq
instance Show GroupId where
    show (GroupId u) = u
instance IsString GroupId where
    fromString str = GroupId str

data DiskId  = DiskId String deriving Eq
instance Show DiskId  where
    show (DiskId u) = u
instance IsString DiskId where
    fromString str = DiskId str

data ImageId = ImageId String deriving Eq
instance Show ImageId where
    show (ImageId u) = u
instance IsString ImageId where
    fromString str = ImageId str

-- DiskUuid: each uuid uniquely represents the logical contents of a virtual disk
data DiskUuid  = DiskUuid String deriving Eq
instance Show DiskUuid  where
    show (DiskUuid u) = u
instance IsString DiskUuid where
    fromString str = DiskUuid str

-- ImageUuid: each uuid uniquely represents a VHD image in a chain
data ImageUuid = ImageUuid String deriving Eq
instance Show ImageUuid where
    show (ImageUuid u) = u
instance IsString ImageUuid where
    fromString str = ImageUuid str

data TransferCtxID = TransferCtxID String deriving (Eq)

instance IsString TransferCtxID where
    fromString s = TransferCtxID s

data DisableXferCache  = DisableDvdCache
                       | DisableUsbCache
                       | NoDisable
                       deriving (Eq, Show)

data CryptoSpec = CryptoSpec {
      cryptoServerCertPath :: FilePath
    , cryptoClientCertPath :: FilePath
    , cryptoClientKeyPath  :: FilePath
    } deriving (Eq, Show)

data CommonTransferOptions = CommonTransferOptions {
      chunkSize         :: Int
    , logLevel          :: Int
    , verbose           :: Bool
    , lowspeedLimit     :: Int
    , lowspeedTime      :: Int
    , connectTimeout    :: Int
    } deriving (Eq, Show)

data TransferOptions =
    UploadOptions {
      enableCompaction :: Bool
    , commonOptions    :: CommonTransferOptions
    } |
    DownloadOptions {
      encryptLocalImage :: Bool
    , enableDvdCache    :: Bool
    , enableUsbCache    :: Bool
    , forceLocalCheckSum :: Bool
    , commonOptions     :: CommonTransferOptions
    } deriving (Eq, Show)

data Image = Image {
      imageUuid              :: ImageUuid
    , imageOrdinal           :: Int
    , imageDiskId            :: DiskId
    , imageUrl               :: String
    , imageCrypto            :: CryptoSpec
    , imageEncryptLocalImage :: Bool
    , imageTransferUrl       :: String
    , imageTransferCtxId     :: TransferCtxID
    } deriving (Eq)

mkId :: IsString a => [String] -> a
mkId parts =
    fromString $ concat $ intersperse "__" parts

imageId :: Image -> ImageId
imageId img =
    mkId [show disk_id, show img_uuid]
    where
      disk_id = imageDiskId img
      img_uuid = imageUuid img

data Disk = Disk {
      diskUuid      :: DiskUuid
    , diskGroupId   :: GroupId
    , diskDirection :: Direction
    } deriving (Eq)

diskId :: Disk -> DiskId
diskId dsk =
    mkId [show group_id, show disk_uuid]
    where
      group_id = diskGroupId dsk
      disk_uuid = diskUuid dsk

data ImageFailureReason = ImageFailUnknown             --An unknown failure
                        | ImageFailFileAccessError     --Image file could not be opened (filer could have gone down, etc.)
                        | ImageFailInvalidRequest      --Request was malformed
                        | ImageFailFileNotUnique       --Specified filename is not unique in the image repository
                        | ImageFailFileNotExists       --File does not exist in the specified image repository
                        | ImageFailFileNotTracked      --File is not tracked in the backend database
                        | ImageFailInsufficientSpace   --Not enough disk space in image repository to complete the transfer
                        | ImageFailChecksumMismatch    --Checksums on client/server don't match
                        | ImageFailInvalidImageState   --Image file in bad state (FINAL for upload, PARTIAL for download)
                        | ImageFailIncompatibleVersion --Client version is not compatible with the server version
                        | ImageFailInvalidKey          --Key validation of encrypted image failed
                        | ImageFailAccessDenied        --Cannot grant access to the file (anymore)
                        | ImageFailHTTPNotAllowed      --HTTP transfer requested but not HTTP not configured
                        | ImageFailTooManyXfers        --Too many transfers are happening right now
                        | ImageFailInternalServerError --Internal server error
                        | ImageFailCoalesced           --Image was coalesced on the server during transfer
                        | ImageFailLocalChecksumFailureDvd   --Vhd on DVD is corrupted
                        | ImageFailLocalChecksumFailureUsb   --Vhd on USB is corrupted
                        | ImageFailLocalChecksumNotFoundDvd  --Could not find local checksum file on DVD
                        | ImageFailLocalChecksumNotFoundUsb  --Could not find local checksum file on USB
                        | ImageFailLocalChecksumPartFailDvd  --Local Checksum Partial Failure on DVD
                        | ImageFailLocalChecksumPartFailUsb  --Local Checksum Partial Failure USB
                        | ImageFailLocalChecksumMalformedDvd --Local Checksum Malformed on DVD
                        | ImageFailLocalChecksumMalformedUsb --Local Checksum Malformed on USB
                          deriving ( Eq, Show )

stringOfReason ImageFailUnknown = "unknown"
stringOfReason ImageFailFileAccessError = "file-access-error"
stringOfReason ImageFailInvalidRequest = "invalid-request"
stringOfReason ImageFailFileNotUnique = "file-not-unique"
stringOfReason ImageFailFileNotExists = "file-not-exists"
stringOfReason ImageFailFileNotTracked = "file-not-tracked"
stringOfReason ImageFailInsufficientSpace = "insufficient-space"
stringOfReason ImageFailChecksumMismatch = "checksum-mismatch"
stringOfReason ImageFailInvalidImageState = "invalid-image-state"
stringOfReason ImageFailIncompatibleVersion = "incompatible-version"
stringOfReason ImageFailInvalidKey = "invalid-key"
stringOfReason ImageFailAccessDenied = "access-denied"
stringOfReason ImageFailHTTPNotAllowed = "http-not-allowed"
stringOfReason ImageFailTooManyXfers = "too-many-xfers"
stringOfReason ImageFailInternalServerError = "internal-server-error"
stringOfReason ImageFailCoalesced = "image-coalesced"
stringOfReason ImageFailLocalChecksumFailureDvd = "local-checksum-failure-dvd"
stringOfReason ImageFailLocalChecksumFailureUsb = "local-checksum-failure-usb"
stringOfReason ImageFailLocalChecksumNotFoundDvd = "local-checksum-not-found-dvd"
stringOfReason ImageFailLocalChecksumNotFoundUsb = "local-checksum-not-found-usb"
stringOfReason ImageFailLocalChecksumPartFailDvd = "local-checksum-part-fail-dvd"
stringOfReason ImageFailLocalChecksumPartFailUsb = "local-checksum-part-fail-usb"
stringOfReason ImageFailLocalChecksumMalformedDvd = "local-checksum-malformed-dvd"
stringOfReason ImageFailLocalChecksumMalformedUsb = "local-checksum-malformed-usb"

reasonOfString "unknown" = ImageFailUnknown
reasonOfString "file-access-error" = ImageFailFileAccessError
reasonOfString "invalid-request" = ImageFailInvalidRequest
reasonOfString "file-not-unique" = ImageFailFileNotUnique
reasonOfString "file-not-exists" = ImageFailFileNotExists
reasonOfString "file-not-tracked" = ImageFailFileNotTracked
reasonOfString "insufficient-space" = ImageFailInsufficientSpace
reasonOfString "checksum-mismatch" = ImageFailChecksumMismatch
reasonOfString "invalid-image-state" = ImageFailInvalidImageState
reasonOfString "incompatible-version" = ImageFailIncompatibleVersion
reasonOfString "invalid-key" = ImageFailInvalidKey
reasonOfString "access-denied" = ImageFailAccessDenied
reasonOfString "http-not-allowed" = ImageFailHTTPNotAllowed
reasonOfString "too-many-xfers" = ImageFailTooManyXfers
reasonOfString "internal-server-error" = ImageFailInternalServerError
reasonOfString "image-coalesced" = ImageFailCoalesced
reasonOfString "local-checksum-failure-dvd" = ImageFailLocalChecksumFailureDvd
reasonOfString "local-checksum-failure-usb" = ImageFailLocalChecksumFailureUsb
reasonOfString "local-checksum-not-found-dvd" = ImageFailLocalChecksumNotFoundDvd
reasonOfString "local-checksum-not-found-usb" = ImageFailLocalChecksumNotFoundUsb
reasonOfString "local-checksum-part-fail-dvd" = ImageFailLocalChecksumPartFailDvd
reasonOfString "local-checksum-part-fail-usb" = ImageFailLocalChecksumPartFailUsb
reasonOfString "local-checksum-malformed-dvd" = ImageFailLocalChecksumMalformedDvd
reasonOfString "local-checksum-malformed-usb" = ImageFailLocalChecksumMalformedUsb
reasonOfString x = error ("unknown failure reason " ++ x)

data ImageState = ImageStopped
                | ImageInProgress
                | ImageNetworkStalled
                | ImageAdmCtlStalled
                | ImagePaused
                | ImageFinished
                | ImageFailed ImageFailureReason
                | ImageSrvCksumInProgress deriving ( Eq, Show )

data DiskState = DiskStopped
               | DiskInProgress
               | DiskNetworkStalled
               | DiskAdmCtlStalled
               | DiskPaused
               | DiskFinished
               | DiskFailed
               | DiskSrvCksumInProgress deriving ( Eq, Show )

diskStateStr DiskStopped = "stopped"
diskStateStr DiskInProgress = "inprogress"
diskStateStr DiskNetworkStalled = "network-stalled"
diskStateStr DiskAdmCtlStalled = "admctl-stalled"
diskStateStr DiskPaused = "paused"
diskStateStr DiskFinished = "finished"
diskStateStr DiskFailed = "failed"
diskStateStr DiskSrvCksumInProgress = "server-checksum-in-progress"

data GroupState = GroupStopped
                | GroupInProgress
                | GroupNetworkStalled
                | GroupAdmCtlStalled
                | GroupPaused
                | GroupFinished
                | GroupFailed
                | GroupSrvCksumInProgress deriving ( Eq, Show )

groupStateStr GroupStopped = "stopped"
groupStateStr GroupInProgress = "inprogress"
groupStateStr GroupPaused = "paused"
groupStateStr GroupFailed = "failed"
groupStateStr GroupFinished = "finished"
groupStateStr GroupNetworkStalled = "network-stalled"
groupStateStr GroupAdmCtlStalled = "admctl-stalled"
groupStateStr GroupSrvCksumInProgress = "server-checksum-in-progress"
