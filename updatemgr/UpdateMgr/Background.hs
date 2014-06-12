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

module UpdateMgr.Background where

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Error
import Control.Exception.Lifted
import Tools.Log
import UpdateMgr.Rpc
import UpdateMgr.Error
import UpdateMgr.UpdateMonad
import UpdateMgr.UpdateReqHandler
import UpdateMgr.DbReqHandler
import UpdateMgr.App

type Job = MVar (Maybe ThreadId)

initJob :: IO Job
initJob = newMVar Nothing

cancelJob :: Job -> IO ()
cancelJob job = modifyMVar_ job cancel
  where
    cancel Nothing = return Nothing
    cancel (Just thread) = killThread thread >> return Nothing

backgroundApp :: App () -> App ThreadId
backgroundApp = fork

foregroundUpdate :: Update a -> App a
foregroundUpdate = runner

runner :: Update a -> App a
runner = runUpdate handleUpdateReq handleDbReq

backgroundUpdate :: Job -> Update () -> App ()
backgroundUpdate job action = modifyMVar_ job $ \id -> case id of
    Just _  -> throwError (localE BackgroundOpAlreadyRunning)
    Nothing -> liftM Just . backgroundApp $ do
        info "starting bg operation."
        finally (runner action) (swapMVar job Nothing)

foregroundUpdate' :: AppState -> Update a -> Rpc a
foregroundUpdate' state action = runApp state (foregroundUpdate action)

backgroundUpdate' :: AppState -> Job -> Update () -> Rpc ()
backgroundUpdate' state job action = runApp state (backgroundUpdate job action)
