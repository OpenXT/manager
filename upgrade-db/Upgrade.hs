--
-- Copyright (c) 2014 Citrix Systems, Inc.
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

module Main where

import Prelude hiding (catch)
import Control.Monad
import Control.Exception
import Control.Applicative

import System
import System.IO
import Directory

import ShellTools
import InformTools

import UpgradeEngine
import Migrations

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T


-- MODIFY THIS WHEN FORMAT CHANGES
latestVersion :: Int
latestVersion = 30
----------------------------------

dbdRunning :: IO Bool
dbdRunning = do
  pid <- spawnShell' "pidof dbd"
  return $ pid /= Nothing

backupConfig :: IO ()
backupConfig = do
  current <- getCurrentVersion
  inform $ "Backing up config version " ++ show current
  safeSpawnShell $ "mkdir -p " ++ dstPath current
  safeSpawnShell $ "rsync -avz --exclude 'backups' /config/ " ++ dstPath current
  return ()
  where
    dstPath v = "/config/backups/" ++ show v

performMigration :: Migration -> IO ()
performMigration m = do
  inform $ "Migrating " ++ show (sourceVersion m) ++ " -> " ++ show (targetVersion m)
  runMigrate m

upgradeTo :: Int -> IO ()
upgradeTo target = do
  current <- getCurrentVersion
  when (target /= current) $
       do let m = getMigrationFromVer current
          performMigration m
          upgradeTo target

rollback :: Int -> IO ()
rollback version = do
  inform $ "Rolling back changes to version " ++ show version
  safeSpawnShell $ "rsync -avz --delete --exclude 'backups' /config/backups/" ++ show version ++ "/ /config"
  inform $ "Rollback done. Database was NOT upgraded."
  return ()

--
-- build comparison stuffs
--
data BuildInfo = BuildInfo {
      biBuildNum        :: String
    , biBuildDate       :: String
    , biBuildBranch     :: String
    , biVersion         :: String
    , biRelease         :: String
    }

readBuildInfo :: String -> IO BuildInfo
readBuildInfo filename =
    parse <$> readFile filename
  where
    parse  = fromMap . toMap . lines
    toMap  = foldl' insert M.empty
    insert m line = case map strip . split '=' $ line of
                      [k,v] -> M.insert k v m
                      _     -> m
    fromMap m = BuildInfo {
                  biBuildNum    = maybe "" id (M.lookup "build" m)
                , biBuildDate   = maybe "" id (M.lookup "build_date" m)
                , biBuildBranch = maybe "" id (M.lookup "build_branch" m)
                , biVersion     = maybe "" id (M.lookup "version" m)
                , biRelease     = maybe "" id (M.lookup "release" m)
                }

wipeShaSums :: IO ()
wipeShaSums =
    do inform "clearing hash sums from all measured disks"
       xformVmJSON wipe
    where
      wipe vm_tree        = jsMapChildren wipe_disk "/config/disk" vm_tree
      wipe_disk disk_tree = jsRm "/sha1sum" disk_tree

wipeSuspendImages :: IO ()
wipeSuspendImages =
    do e <- doesFileExist uivmp
       when e $ do
         inform $ "removing UIVM suspend image: " ++ show uivmp
         removeFile uivmp
    where
      uivmp = "/storage/uivm/uivm-suspend-image"

-- Split a list over an element
split :: (Eq a) => a -> [a] -> [[a]]
split sep xs =
    let (y,ys) = span (/= sep) xs in
    case ys of
      [] -> [y]
      zs -> y : split sep (tail zs)

strip = T.unpack . T.strip . T.pack

buildUpgrade :: IO ()
buildUpgrade = do
  have_etc_conf  <- doesFileExist etc_path
  have_conf_conf <- doesFileExist config_path
  rehash <- doesFileExist rehash_path
  when ( not have_etc_conf ) $ error "fatal error, /etc/xenclient.conf does not exist!"
  if not have_conf_conf
     then do_it
     else do bi  <- readBuildInfo etc_path
             bi' <- readBuildInfo config_path
             if biBuildNum bi /= biBuildNum bi'
                then do_it
                else inform $ "build number has not changed"
  when rehash $ rehash_vms >> removeFile rehash_path
  where
    etc_path = "/etc/xenclient.conf"
    config_path = "/config/xenclient.conf"
    rehash_path = "/config/rehash-vms"
    do_it = do
      inform $ "build number has changed - performing appropriate actions"      
      wipeSuspendImages
      safeSpawnShell $ "cp " ++ etc_path ++ " " ++ config_path
      inform $ "appropriate performed"
    rehash_vms = do
      inform $ "rehash flag present"
      wipeShaSums

main = do
  dbd <- dbdRunning
  when dbd $ inform "Cannot upgrade while database daemon 'dbd' is running" >> exitFailure

  current <- getCurrentVersion
  if ( current < latestVersion )
     then do backupConfig
             upgradeTo latestVersion
                           `catch` (errorRollback current)
     else inform $ "Database is already in latest version, " ++ show latestVersion

  -- some upgrade stuff which does not relate to db version
  buildUpgrade

  where
    errorRollback :: Int -> SomeException -> IO ()
    errorRollback version err = do
         inform $ "Error: " ++ show err
         rollback version
