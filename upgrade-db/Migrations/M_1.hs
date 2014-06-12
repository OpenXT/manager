--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

module Migrations.M_1 (migration) where

import System.IO.Unsafe
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.List
import FileTools
import ShellTools
import UpgradeEngine

migration = Migration {
              sourceVersion = 1
            , targetVersion = 2
            , actions = act
            }

act :: IO ()
act = do
  -- have to remove uivm template because the uuid has chanegd
  liftIO $ removeExistingFile "/config/vms/00000000-0000-0000-0000-000000000002.db"
  -- helpful hint..
  hh <- helpfulHint
  xformPrimaryJSON $ jsSet "/xenmgr/showMsgOnVmStart" (JSBool hh)
  -- wallpaper
  wp <- wallpaper
  case wp of
    Nothing -> return ()
    Just p  -> xformPrimaryJSON $ jsSet "/xenmgr/wallpaper" (jsBoxString p)
  -- backend
  backendUpgrade

wallpaper :: IO (Maybe String)
wallpaper = doesFileExist "/etc/xenmgr.conf" >>= \ex -> case ex of False -> return Nothing
                                                                   True  -> parse
    where parse = readFile "/etc/xenmgr.conf" >>= return . scan . lines
          scan [] = Nothing
          scan (l:ls) = case words l of
                          ["wallpaper", "=", path] -> Just path
                          _ -> scan ls


helpfulHint :: IO Bool
helpfulHint = doesFileExist "/etc/xenmgr.conf" >>= \ex -> case ex of False -> return True
                                                                     True  -> parse
    where parse = readFile "/etc/xenmgr.conf" >>= return . not . is_false . lines
          is_false lines = "showMsgOnVmStart = false" `elem` lines

backendUpgrade :: IO ()
backendUpgrade = do
    exists <- doesFileExist bedStore
    when ( exists ) $ readJSONFile bedStore >>= \s -> xformPrimaryJSON $ migrateBedStore s
    xformPrimaryJSON $ \tree -> jsSet (backendPath ++ "/bed-version") (jsBoxString "1") tree
    xformVmJSON $ vmUpgrade
  where
    bedStore = "/config/backend/bed.store"
    backendPath = "/com.citrix.xenclient.backend"

    -- copy values from bed.store into the main db config
    migrateBedStore store tree =
        cp "/device_id" .
        cp "/hardware_info" .
        cp "/server_url" .
        cp "/secure_device_token" .
        cp "/userid" .
        cp "/username" .
        cp "/user_privilege_mask" .
        cp "/owner" .
        cp "/transmitter_hostname" $
        tree
      where
        cp path tree =
            case jsGet path store of
              Nothing -> tree
              Just v  -> jsSet (backendPath ++ path) v tree

    -- only xform if have appliance name
    vmUpgrade tree | jsGet "/backend/appliance_name" tree == Nothing  = tree
                   | otherwise                                        = scrapeFields . upgradeManifest . upgradeDisks $ tree

    scrapeFields tree
        = jsRm "/backend/appliance_name" .
          jsRm "/backend/appliance_version" $
          tree

    upgradeDisks tree
        = jsMapChildren f "/config/disk" tree
          where f tree | jsGet "/devtype" tree == (Just . jsBoxString $ "disk")
                           = jsSet "/disktype" (jsBoxString "system") tree
                       | otherwise
                           = tree

    upgradeManifest tree
        = jsModify disks (manifestPath ++ "/disk_manifests") .
          jsSet (manifestPath ++ "/appliance/basedOnVersion") JSNull $
          tree
                 where manifestPath = "/backend/appliance_manifest"
                       disks (JSArray dms)  = JSArray . map (jsModify images "image_manifests") $ dms
                       disks _              = error "expected array of disk manifests"
                       images (JSArray ims) = JSArray . map (jsModify image "disk_image") $ ims
                       images _             = error "expected array of image manifests"
                       image tree  = jsModify conv "cTime" $ tree
                       conv ctimeT = JSRational True $ convDate (jsUnboxString ctimeT)

convDate :: String -> Rational
convDate str = unsafePerformIO $ do
    str' <- safeSpawnShell cmd
    let d = read str' :: Double
    return . realToFrac $ d
  where
    cmd = "ruby -e \"require 'time';print Time.parse('" ++ str ++ "').to_f\""

readJSONFile :: FilePath -> IO JSValue
readJSONFile path = do
  contents <- readFile path
  case decodeStrict $ contents of
    Error msg -> error $ "malformed JSON file: " ++ msg
    Ok json   -> return json
