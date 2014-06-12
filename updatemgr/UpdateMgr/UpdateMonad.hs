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

{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveFunctor, RankNTypes, NoMonomorphismRestriction #-}
module UpdateMgr.UpdateMonad
    ( Update(..)
    , runUpdate
    , newEmptyDirectory, rmDirectory, rmFile, mv, fileContents, fileExists, fileLen, computeSHA256, unTarGZ
    , beginDownload, resumeDownload, cancelDownload
    , getNextDownloadEvent, getCurrentDownload, setCurrentDownload
    , getCurrentDownloadSpeed, setCurrentDownloadSpeed
    , queryUpdatesPolicy, areGuestVmsRunning
    , notifyUpdateDownloadProgress, notifyUpdateStateChange
    , waitNetworkAvailable
    , safeShellExecute, safeShellExecuteAndLogOutput
    , logInfo
    , rebootHost, shutdownHost
    , dbMaybeRead, dbWrite, dbMaybeWrite, dbRm, dbList
    , dbReadWithDefault
    , module X
    ) where

import Control.Applicative as X
import Control.Monad
import Data.Maybe
import Control.Monad.Prompt as X
import Control.Monad.Error as X

import UpdateMgr.Error
import UpdateMgr.Types
import UpdateMgr.Rpc
import UpdateMgr.UpdateReq as X
import UpdateMgr.DbReq as X
import UpdateMgr.App


instance Applicative Update where
    pure  = return
    (<*>) = ap

-- rethrow left Either prompt computation in Error monad if necessary
promptU p = from =<< (Update . lift $ prompt p) where
    from (Right r) = return r
    from (Left er) = throwError er

promptD p = Update . lift . lift $ prompt p

runUpdate :: (forall r. UpdateReq r -> App r)
          -> (forall r. DbReq r -> Rpc r)
          -> Update a
          -> App a
runUpdate dispatcher_upd dispatcher_db u = do
  let errM = runErrorT . unUpdate $ u
  r <- runPromptT
         return -- handler when there's no more computations
         (\p cont -> dispatcher_upd p >>= cont) -- handler for computations
         (\m cont -> liftRpc (runPromptM dispatcher_db m) >>= cont) -- handler for lifted computations
         errM -- after running error monad
  case r of
    Left er -> throwError er
    Right v -> return v

newEmptyDirectory = promptU . NewEmptyDirectory
rmDirectory = promptU . RmDirectory
rmFile = promptU . RmFile
mv from to = promptU $ MoveFile from to
fileContents = promptU . FileContents
fileExists = promptU . FileExists
fileLen = promptU . FileLength
computeSHA256 = promptU . ComputeSHA256
unTarGZ file to = promptU $ UnTarGZ file to
beginDownload u p = promptU $ BeginDownload u p
resumeDownload u p = promptU $ ResumeDownload u p
cancelDownload = promptU . CancelDownload
getNextDownloadEvent = promptU . GetNextDownloadEvent
getCurrentDownload = promptU GetCurrentDownload
setCurrentDownload = promptU . SetCurrentDownload
getCurrentDownloadSpeed = promptU GetCurrentDownloadSpeed
setCurrentDownloadSpeed = promptU . SetCurrentDownloadSpeed
queryUpdatesPolicy = promptU QueryUpdatesPolicy
areGuestVmsRunning = promptU AreGuestVmsRunning
notifyUpdateStateChange = promptU . NotifyUpdateStateChange
notifyUpdateDownloadProgress = promptU . NotifyUpdateDownloadProgress
safeShellExecute = promptU . SafeShellExecute
safeShellExecuteAndLogOutput = promptU . SafeShellExecuteAndLogOutput
logInfo = promptU . LogInfo
rebootHost = promptU RebootHost
shutdownHost = promptU ShutdownHost
waitNetworkAvailable = promptU WaitNetworkAvailable

dbMaybeRead = promptD . DbMaybeRead
dbReadWithDefault d = fmap (fromMaybe d) . dbMaybeRead
dbWrite p v = promptD $ DbWrite p v
dbRm = promptD . DbRm
dbList = promptD . DbList

-- ToDo: Perhaps take from a library?
dbMaybeWrite = maybe <$> dbRm <*> dbWrite
