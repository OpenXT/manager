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

module UpdateMgr.Poller where

import Control.Monad
import Control.Applicative
import Control.Concurrent.Lifted
import Control.Concurrent.MVar.Lifted
import Control.Monad.Trans.Control
import qualified Control.Exception as E

import System.Timeout
import System.Posix.Files
import System.Directory

import Tools.File
import System.IO.Error
import System.Posix.IO

import UpdateMgr.Logic
import UpdateMgr.UpdateMonad

import Tools.Log
import Tools.IfM
import Tools.Misc

-- Do not de-reference symlinks.  Just check if the symlink itself exists.
doesFileExist' :: FilePath -> IO Bool
doesFileExist' fp = E.handle h (True <$ getSymbolicLinkStatus fp)
  where h :: IOError -> IO Bool
        h _ = return False

updateStaging = "/storage/update-staging"

liftBaseOp' :: MonadBaseControl b m
               => ((x -> b ()) -> b a)
               ->  (x -> m ()) -> m a
liftBaseOp' f m = liftBaseWith $ \runInBase -> f $ void . runInBase . m


monitorSymlink runner = fork $ liftBaseOp' (monitorSymlink' . action) (runner . applyUpdateFromTarball)

-- TODO: Consider, if we need to be able to shut this monitor down?
-- TODO: get rid of literal constants ("/storage[/update-staging]", and for timeout)
-- TODO: review events to monitor for the different watches
monitorSymlink' action = forever $ do
    action
    threadDelay (30 * 10^6)

action tryUpdate = do
    tarballs <- E.handle h $ filesInDir updateStaging
    case tarballs of
        [] -> return ()
        (_:_:_) -> debug $ "Poller/action: Found more than one file ("++ show tarballs++")"
        [tarball] -> do
            debug $ "Poller/action: Found just one file ("++ show tarball++").  Good!"
            -- TODO: Introduce locking.  (No time for testing it now,
            -- but we only use the file with the tar command, which
            -- will keep a file-descriptor open, and so we don't care
            -- if somebody is removing the directory entry.)
            id $ -- withLock tarball Exclusive NoBlock $
                tryUpdate tarball
  where h :: IOError -> IO [FilePath]
        h e | isDoesNotExistError e = return []
              -- TODO: Consider using internal error handling.  (Instead of just dying?)
            | otherwise = E.throw e
