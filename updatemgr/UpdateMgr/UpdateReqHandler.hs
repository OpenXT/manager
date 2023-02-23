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

{-# LANGUAGE GADTs, PatternGuards #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables, ExistentialQuantification, RankNTypes, FlexibleContexts #-}
module UpdateMgr.UpdateReqHandler (handleUpdateReq, DownloadHandle()) where


import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Data.String
import Data.IORef
import System.Timeout
import System.Posix.Files
import System.Directory
import qualified Data.Text.Lazy as TL
import System.Xen.Store
import System.Process
import System.IO
import System.INotify
import System.Exit
import Data.Time.Clock
import Control.Concurrent.Lifted
import Control.Concurrent.MVar.Lifted
import Control.Monad.Trans.Control
import Control.Monad.Loops
import qualified Control.Exception.Lifted as E

import Rpc.Autogen.UpdatemgrNotify
import Rpc.Autogen.XenmgrClient
import Rpc.Autogen.XenmgrVmClient
import Rpc.Autogen.XenmgrHostClient
import UpdateMgr.UpdateReq
import UpdateMgr.Types
import UpdateMgr.Rpc
import UpdateMgr.App
import UpdateMgr.Error
import UpdateMgr.Curl

import Tools.Log
import Tools.Misc
import Tools.Text
import Tools.Process
import Tools.IfM
import Network.HTTP

catchingLiftIO :: IO a -> App a
catchingLiftIO a = from =<< liftIO (E.try a) where
    from (Right x) = return x
    from (Left er)
        | Just E.ThreadKilled <- E.fromException er = throwError . localE $ DownloadWasCancelled
        | otherwise = throwError . localE $ (InternalError $ show (er :: E.SomeException))

updatemgrObjectPath :: ObjectPath
updatemgrObjectPath = fromString "/"

-- FIXME types
xenmgrObjectPath :: String
xenmgrObjectPath = "/"

xenmgrHostObjectPath :: String
xenmgrHostObjectPath = "/host"

xenmgrService :: String
xenmgrService = "com.citrix.xenclient.xenmgr"

eitherErr a = (Right <$> a) `catchError` (return . Left)

handleUpdateReq :: UpdateReq r -> App r
handleUpdateReq (NewEmptyDirectory path) = eitherErr . catchingLiftIO . void $ do
    whenM (doesDirectoryExist path) . void .
         safeSpawnShell $ "rm -rf " ++ path
    safeSpawnShell $ "mkdir -p " ++ path

handleUpdateReq (RmFile path) = eitherErr . catchingLiftIO $
    whenM (doesFileExist path) $ removeFile path

handleUpdateReq (RmDirectory path) = eitherErr . catchingLiftIO $
    whenM (doesDirectoryExist path) . void $
         safeSpawnShell $ "rm -rf " ++ path

handleUpdateReq (FileExists path) = eitherErr . catchingLiftIO $
    doesFileExist path

handleUpdateReq (FileLength path) = eitherErr . catchingLiftIO $
    ifM (doesFileExist path)
        (fromIntegral . fileSize <$> getFileStatus path)
        (return 0)

--- Strict!
handleUpdateReq (FileContents path) = eitherErr . catchingLiftIO $ do
  s <- readFile path
  length s `seq` return s

handleUpdateReq (BeginDownload url path)  = eitherErr . catchingLiftIO $ curlBeginDownload url path
handleUpdateReq (ResumeDownload url path) = eitherErr . catchingLiftIO $ curlResumeDownload url path
handleUpdateReq (GetNextDownloadEvent h)  = eitherErr . catchingLiftIO $ curlGetNextDownloadEvent h
handleUpdateReq (CancelDownload h) = eitherErr . catchingLiftIO $ curlCancelDownload h

handleUpdateReq (UnTarGZ from to) = eitherErr . catchingLiftIO . void $ do
    info $ "request to unpack " ++ show from ++ " to " ++ show to
    E.handle x ((info =<<) $ safeSpawnShell $ "tar -C " ++ to ++ " -xf " ++ from)
    info $ "unpacked " ++ show from ++ " to " ++ show to
  where x :: E.SomeException -> IO ()
        x e = info $ "error while unpacking: " ++ show e

handleUpdateReq (ComputeSHA256 path) = eitherErr . catchingLiftIO $ do
    info $ "computing SHA256 for " ++ show path
    liftM parse . safeSpawnShell $ "openssl dgst -sha256 " ++ path
    where
      parse str =
        let hexstr = head . tail . words $ str
            Just v = maybeRead $ "0x" ++ hexstr in
        v

handleUpdateReq (NotifyUpdateStateChange s) = eitherErr . liftRpc $
    notifyComCitrixXenclientUpdatemgrUpdateStateChange updatemgrObjectPath (updateStateStr s)

handleUpdateReq (NotifyUpdateDownloadProgress p) = eitherErr . liftRpc $
    notifyComCitrixXenclientUpdatemgrUpdateDownloadProgress updatemgrObjectPath (realToFrac $ dlComplete p) (realToFrac $ dlSpeed p)

handleUpdateReq (SafeShellExecute cmd) = eitherErr . catchingLiftIO $ safeSpawnShell cmd
handleUpdateReq (SafeShellExecuteAndLogOutput cmd) =
    eitherErr . catchingLiftIO $
      runInteractiveCommand cmd >>= \ (_, stdout, stderr, h) ->
          do hSetBuffering stdout NoBuffering
             hSetBuffering stderr NoBuffering
             contents_mv <- newEmptyMVar
             fork . void $ consume_lines stderr warn
             fork $ putMVar contents_mv =<< consume_lines stdout info
             contents <- takeMVar contents_mv
             -- force evaluation of contents
             exitCode <- contents `seq` waitForProcess h
             case exitCode of
               ExitSuccess -> return contents
               _           -> error  $ "shell command: " ++ cmd ++ " FAILED."

    where
      consume_lines h feed = go h [] where
          go h ls = continue =<< E.try (hGetLine h)
              where continue :: Either E.SomeException String -> IO String
                    continue (Right l) = feed l >> go h (l:ls)
                    continue (Left _)  = return . unlines . reverse $ ls

handleUpdateReq (GetCurrentDownload) = eitherErr $
    catchingLiftIO . readIORef . appCurrentDownload =<< ask

handleUpdateReq (SetCurrentDownload d) = eitherErr $
    catchingLiftIO . flip writeIORef d . appCurrentDownload =<< ask

handleUpdateReq (GetCurrentDownloadSpeed) = eitherErr $
    catchingLiftIO . readIORef . appCurrentDownloadSpeed =<< ask

handleUpdateReq (SetCurrentDownloadSpeed s) = eitherErr $
    catchingLiftIO . flip writeIORef s . appCurrentDownloadSpeed =<< ask

handleUpdateReq (LogInfo s) = eitherErr . catchingLiftIO $ info s

handleUpdateReq QueryUpdatesPolicy = eitherErr . liftRpc $
    comCitrixXenclientXenmgrConfigGetOtaUpgradesAllowed xenmgrService xenmgrObjectPath

handleUpdateReq AreGuestVmsRunning = eitherErr . liftRpc $
    anyM is_running_guest =<< comCitrixXenclientXenmgrListVms xenmgrService xenmgrObjectPath
    where
      is_running_guest :: ObjectPath -> Rpc Bool
      is_running_guest obj = andM [
          ("stopped"/=) <$> comCitrixXenclientXenmgrVmGetState xenmgrService (strObjectPath $ obj)
        , ("svm"==) <$> comCitrixXenclientXenmgrVmGetType xenmgrService (strObjectPath $ obj)]

handleUpdateReq RebootHost = eitherErr . liftRpc $
    comCitrixXenclientXenmgrHostReboot xenmgrService xenmgrHostObjectPath

handleUpdateReq ShutdownHost = eitherErr . liftRpc $
    comCitrixXenclientXenmgrHostShutdown xenmgrService xenmgrHostObjectPath

handleUpdateReq WaitNetworkAvailable = eitherErr . liftIO $ do
        info "waiting for network availability..."
        xsWaitForPure "/xenmgr/network-available" (== "1")
        info "have network"

-- TODO: Handle escaping and errors.
handleUpdateReq (MoveFile from to) = eitherErr . catchingLiftIO . void $
                                     safeSpawnShell $ "mv " ++ from ++ " " ++ to

xsWaitForPure :: XsPath -> (XsData -> Bool) -> IO ()
xsWaitForPure path predicate = xsWaitFor path predicate'
    where predicate' = (fmap.fmap.fmap) predicate xsRead

xsWaitFor :: XsPath -> (XsHandle -> XsPath -> IO Bool) -> IO ()
xsWaitFor path predicate = withXS $ \handle -> void $ do
  semaphore <- newEmptyMVar
  uuid <- uuidGen
  let callback _ = whenM (predicate handle path) $ putMVar semaphore () >>
                                                   xsUnwatch handle path uuid

  xsWatch handle path uuid callback
  takeMVar semaphore

-- ToDo: Take from Misc.hs or so.
uuidGen :: IsString a => IO a
uuidGen = fromString . strip <$> readFile "/proc/sys/kernel/random/uuid"
