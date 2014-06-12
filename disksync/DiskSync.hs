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

module DiskSync
    ( DiskSync
    , ActiveTask (..)
    , AppContext
    , createAppContext
    , getAppContext
    , getActiveTasks
    , newActiveTask
    , addActiveTask
    , rmActiveTask
    , findActiveTask
    , progressActiveTask
    , resultActiveTask
    , admctlActiveTask
    , serverCksumActiveTask
    , liftRpc
    , runDiskSync
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad.Trans
import Data.Maybe

import Rpc
import Errors
import Types
import VhdSync

data AppState = AppState
              { appActiveTasks :: [ActiveTask] }

type AppContext = MVar AppState

createAppContext :: IO AppContext
createAppContext = newMVar emptyAppState

data ActiveTask = ActiveTask {
      atImage :: Image
    , atDisk  :: Disk
    , atSync  :: SyncProcess
    , atProgress :: Maybe (SyncStage, SyncProgress)
    , atResult :: Maybe (SyncResultDetails)
    , atAdmctl :: Maybe (SyncAdmctlDetails)
    , atSrvcksum :: Maybe (SyncSrvcksumDetails)
    }

-- the primary application monad, on top of rpc one
newtype DiskSync a =
    DiskSync (ReaderT AppContext (StateT AppState Rpc) a)

             deriving (Monad,
                       MonadReader AppContext,
                       MonadState AppState,
                       MonadError DiskSyncError,
                       MonadIO)

runDiskSync :: AppContext -> DiskSync a -> Rpc a
runDiskSync ctx (DiskSync sm) =
    do rpc_ctx <- rpcGetContext
       -- have to be careful to not deadlock on the mvar if exception ensures, hence
       -- the crazy usage of modifyMVar
       -- FIXME: this is terrible though with the ugly tunneling of error
       status <- liftIO . modifyMVar ctx $ \s0 -> do
                   status <- rpc rpc_ctx $ runStateT (runReaderT sm ctx) s0
                   case status of
                     Left err -> return (s0, Left err)
                     Right (r, s1) -> return (s1, Right r)
       case status of
         Left err -> throwError err
         Right r -> return r

instance Functor DiskSync where
    fmap = liftM

instance Applicative DiskSync where
    pure  = return
    (<*>) = ap

liftRpc :: Rpc a -> DiskSync a
liftRpc = DiskSync . lift . lift

emptyAppState :: AppState
emptyAppState = AppState []

getAppContext :: DiskSync AppContext
getAppContext = ask

newActiveTask :: Image -> Disk -> SyncProcess -> ActiveTask
newActiveTask image disk sync = ActiveTask image disk sync Nothing Nothing Nothing Nothing

getActiveTasks :: DiskSync [ActiveTask]
getActiveTasks = appActiveTasks <$> get

--- Does the new task have to be appended, or can we cons it t:s instead?
addActiveTask :: ActiveTask -> DiskSync ()
addActiveTask t = modify $ \s -> AppState (appActiveTasks s ++ [t])

findActiveTask :: DiskId-> DiskSync (Maybe ActiveTask)
findActiveTask disk_id =
  fmap (listToMaybe . filter (equal disk_id) . appActiveTasks) get

equal disk_id at = diskId (atDisk at) == disk_id

modifyActiveTask :: DiskId -> (ActiveTask -> ActiveTask) -> DiskSync ()
modifyActiveTask disk_id f =
    modify $ AppState . map mod . appActiveTasks
    where
      mod at | (diskId $ atDisk at) == disk_id = f at
             | otherwise                       = at

progressActiveTask :: DiskId -> SyncStage -> SyncProgress -> DiskSync ()
progressActiveTask disk_id stage pr = modifyActiveTask disk_id $ \at -> at { atProgress = Just (stage,pr) }

resultActiveTask :: DiskId -> SyncResultDetails -> DiskSync ()
resultActiveTask disk_id result = modifyActiveTask disk_id $ \at -> at { atResult = Just result }

admctlActiveTask :: DiskId -> SyncAdmctlDetails -> DiskSync ()
admctlActiveTask disk_id admctl = modifyActiveTask disk_id $ \at -> at { atAdmctl = Just admctl }

serverCksumActiveTask :: DiskId -> SyncSrvcksumDetails -> DiskSync ()
serverCksumActiveTask disk_id srvcksum = modifyActiveTask disk_id $ \at -> at { atSrvcksum = Just srvcksum }

rmActiveTask :: DiskId -> DiskSync ()
rmActiveTask disk_id =
    modify $ AppState . filter (not . equal disk_id) . appActiveTasks
