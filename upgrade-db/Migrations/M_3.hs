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

module Migrations.M_3 (migration) where

import System.IO.Unsafe
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.List
import FileTools
import ShellTools
import UpgradeEngine
import Data.Bits
import Text.Printf
import System.Directory

migration = Migration {
              sourceVersion = 3
            , targetVersion = 4
            , actions = act
            }

act :: IO ()
act = do
  -- remove the swap vhds
  removeExistingFile "/storage/uivm/uivm-swap.vhd"
  removeExistingFile "/storage/ndvm/ndvm-swap.vhd"

  -- erase the AUTH_FLAG_LOCK bit on /platform/flags
  xformPrimaryJSON $ \tree ->
      case jsGet "/platform/flags" tree of
        Nothing -> tree
        Just flags_value ->
            let flags_str = jsUnboxString flags_value
                flags     = read flags_str :: Int
                flags'    = flags `clearBit` 0 in
            jsSet "/platform/flags" (jsBoxString $ show flags') tree
  xformPrimaryJSON $ \tree -> modifyPlatformFlags tree (\flags -> flags `shiftR` 16)
  backendUpgrade

modifyPlatformFlags tree f =
      case jsGet "/platform/flags" tree of
        Nothing -> tree
        Just flags_value ->
            let flags_str = jsUnboxString flags_value
                flags     = read flags_str :: Int
                flags'    = f flags in
            jsSet "/platform/flags" (jsBoxString $ show flags') tree

backendUpgrade :: IO ()
backendUpgrade = do
    keyExists <- doesFileExist keypath
    storageExists <- doesDirectoryExist storage
    when ( not keyExists ) $ createCustomizationsKey
    when ( storageExists && not keyExists ) $ encryptCustomizationsCaches
  where
    keypath = "/config/backend/keys/customizations.key"
    keysize = 256
    storage = "/storage/disks"

    createCustomizationsKey :: IO ()
    createCustomizationsKey = do
        createDirectoryIfMissing True (takeDirectory keypath)
        safeSpawnShell key
        return ()
     where
       encrypter = "ruby /usr/lib/backend/ruby/encrypter.rb"
       key = printf "%s create-key --path %s --size %s --b64encode true" encrypter keypath (show keysize)

    encryptCustomizationsCaches :: IO ()
    encryptCustomizationsCaches = do
        storageFiles <- map (combine storage) `liftM` getDirectoryContents storage
        let caches   = filter isCache storageFiles
        eCaches      <- mapM encryptCache caches
        mapM_ (uncurry renameFile) $ zip eCaches caches
      where
        isCache :: FilePath -> Bool
        isCache f = takeExtension f == ".customizations"

        encryptCache :: FilePath -> IO FilePath
        encryptCache c = safeSpawnShell enc >> return tmp
          where
            tmp = c ++ ".tmp"
            enc = printf "cat %s | openssl enc -aes-128-cbc -pass file:%s -e > %s" c keypath tmp
