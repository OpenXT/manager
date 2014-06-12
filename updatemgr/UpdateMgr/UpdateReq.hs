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

{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveFunctor, RankNTypes, NoMonomorphismRestriction #-}
module UpdateMgr.UpdateReq where

import System.FilePath
import Control.Monad.Prompt
import UpdateMgr.Types
import UpdateMgr.Error
import UpdateMgr.Curl (DownloadHandle, DownloadInitErr)

import Control.Monad.Prompt as X
import Control.Monad.Error as X
import UpdateMgr.DbReq as X
import UpdateMgr.Error

type UpdateReqE a = UpdateReq (Either UpdateMgrError a)

data UpdateReq r where
    NewEmptyDirectory :: FilePath -> UpdateReqE ()
    RmDirectory :: FilePath -> UpdateReqE ()
    RmFile :: FilePath -> UpdateReqE ()
    FileContents :: FilePath -> UpdateReqE String
    FileExists :: FilePath -> UpdateReqE Bool
    FileLength :: FilePath -> UpdateReqE Integer
    SafeShellExecute :: String -> UpdateReqE String
    SafeShellExecuteAndLogOutput :: String -> UpdateReqE String
    ComputeSHA256 :: FilePath -> UpdateReqE Integer
    UnTarGZ :: FilePath -> FilePath -> UpdateReqE ()
    BeginDownload :: URL -> FilePath -> UpdateReqE (Either DownloadInitErr DownloadHandle)
    ResumeDownload :: URL -> FilePath -> UpdateReqE (Either DownloadInitErr DownloadHandle)
    GetNextDownloadEvent :: DownloadHandle -> UpdateReqE DownloadEvent
    CancelDownload :: DownloadHandle -> UpdateReqE ()
    AreGuestVmsRunning :: UpdateReqE Bool
    GetCurrentDownload :: UpdateReqE (Maybe DownloadHandle)
    SetCurrentDownload :: Maybe DownloadHandle -> UpdateReqE ()
    GetCurrentDownloadSpeed :: UpdateReqE Float
    SetCurrentDownloadSpeed :: Float -> UpdateReqE ()
    QueryUpdatesPolicy :: UpdateReqE Bool
    NotifyUpdateStateChange :: UpdateState -> UpdateReqE ()
    NotifyUpdateDownloadProgress :: DownloadProgress -> UpdateReqE ()
    LogInfo :: String -> UpdateReqE ()
    RebootHost :: UpdateReqE ()
    ShutdownHost :: UpdateReqE ()
    WaitNetworkAvailable :: UpdateReqE ()
    MoveFile :: FilePath -> FilePath -> UpdateReqE ()

newtype Update a = Update {
      unUpdate :: ErrorT UpdateMgrError
                    ( PromptT UpdateReq
                      ( Prompt DbReq )
                    ) a
    } deriving (Functor, Monad, MonadError UpdateMgrError)
