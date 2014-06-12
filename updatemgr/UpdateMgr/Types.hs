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

{-# LANGUAGE NoMonomorphismRestriction #-}
module UpdateMgr.Types where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Process
import Control.Arrow
import Control.Applicative
import Text.Printf

type URL = String

data Meta = Meta {
      metaApplicableTo :: [XcVersion]
    , metaDescription :: String
    , metaProduct :: String
    , metaVersion :: XcVersion
    , metaHumaneVersion :: XcHumaneVersion
    , metaFiles :: [FileMeta]
    } deriving (Eq,Show)

data XcVersion = XcVersion String deriving (Eq,Show)
data XcHumaneVersion = XcHumaneVersion String deriving (Eq,Show)

data FileMeta = FileMeta {
      fileName :: String
    , fileRole :: String
    , fileType :: String
    , fileLength :: Integer
    , fileSHA256 :: Integer
    } deriving (Eq,Show)

data FileDownload = FileDownload {
      downloadURL :: URL
    , downloadPath :: FilePath
    , downloadFileRole :: FileRole
    , downloadLength :: Integer
    , downloadSHA256 :: Integer
    } deriving (Eq,Show)

data DownloadEvent = DownloadFinished
                   | DownloadCancelled
                   | DownloadError
                   | DownloadProgressed DownloadProgress
                     deriving (Eq,Show)
data DownloadProgress =
     DownloadProgress { dlComplete :: Float
                      , dlSpeed :: Float } -- Bytes / second
     deriving (Eq)

instance Show DownloadProgress where
    show p = printf "%d%% %.2f MB/s" (round (dlComplete p * 100) :: Int) (dlSpeed p / 1024 / 1024)

data FileRole = ControlFile | OtherFile String
              deriving (Eq,Show)

data UpdateApplicability = CanUpgrade | CannotUpgrade | UpToDate deriving (Eq,Ord,Show,Enum)
updateApplicabilityStrs = Map.fromList $ (updateApplicabilityStr &&& id) <$> apps
    where apps = enumFrom CanUpgrade
updateApplicabilityFromStr str = str `Map.lookup` updateApplicabilityStrs

updateApplicabilityStr CanUpgrade = "can-upgrade"
updateApplicabilityStr CannotUpgrade = "cannot-upgrade"
updateApplicabilityStr UpToDate = "up-to-date"

data UpdateState = NoUpdate | DownloadingReleasesInfo | DownloadedReleasesInfo
                 | DownloadingMeta | DownloadedMeta
                 | DownloadingFiles | DownloadedFiles
                 | Applying | Failed
                 | Done
                 deriving (Eq,Ord,Show,Enum)
updateStateStrs = Map.fromList $ (updateStateStr &&& id) <$> states
    where states = enumFrom NoUpdate

updateStateStr NoUpdate = ""
updateStateStr DownloadingReleasesInfo = "downloading-releases-info"
updateStateStr DownloadedReleasesInfo = "downloaded-releases-info"
updateStateStr DownloadingMeta = "downloading-meta"
updateStateStr DownloadedMeta = "downloaded-meta"
updateStateStr DownloadingFiles = "downloading-files"
updateStateStr DownloadedFiles = "downloaded-files"
updateStateStr Applying = "applying"
updateStateStr Failed = "failed"
updateStateStr Done = "done"

updateStateFromStr str = str `Map.lookup` updateStateStrs

concatURL :: URL -> String -> URL
concatURL url s | "/" `isSuffixOf` url = url ++ s
                | otherwise            = url ++ "/" ++ s
