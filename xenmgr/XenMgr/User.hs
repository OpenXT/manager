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

module XenMgr.User ( User
                   , enumActiveUsers
                   , expireUserSession
                   , hasActiveSession
                   ) where

import Control.Monad
import Control.Applicative
import Data.List
import System.IO
import System.FilePath
import System.Directory
import Tools.File
import Tools.Log
import Tools.Process
import XenMgr.Rpc

data User = User { userFolderPath :: FilePath }

instance Show User where
    show u = takeBaseName . userFolderPath $ u

usersFolder :: FilePath
usersFolder = "/config/sec"

userHashedName :: User -> String
userHashedName user =
    case takeBaseName . userFolderPath $ user of
      ('s' : '-' : name) -> name
      _ -> error "userHashedName: invalid user folder path"

deviceOfMountPath :: FilePath -> IO (Maybe FilePath)
deviceOfMountPath mount_path =
    do entries <- map words . lines <$> readFile "/proc/mounts"
       case filter check entries of
         ((dev_path:_):_) -> return $ Just  dev_path
         _ -> return Nothing
    where
      check (dev_path:mpath:_) = mpath == mount_path
      check _                   = False

enumActiveUsers :: Rpc [User]
enumActiveUsers =
    do exists <- liftIO $ doesDirectoryExist usersFolder
       if (not exists)
          then return []
          else do
            files <- liftIO $ filesInDir usersFolder
            return . map mk_user . filter is_user $ files
       where
         mk_user path = User path
         is_user path = "s-" `isPrefixOf` takeBaseName path

hasActiveSession :: User -> Rpc Bool
hasActiveSession user =
    liftIO (deviceOfMountPath . userFolderPath $ user) >>= return . is_active where
        is_active (Just path) = True
        is_active Nothing     = False

expireUserSession :: User -> Rpc ()
expireUserSession user =
    do active <- hasActiveSession user
       if (not active)
          then warn ("expireUserSession: user " ++ show user ++ " does not have active session")
          else do liftIO . safeSpawnShell $ "sec-umount " ++ userHashedName user
                  return ()
