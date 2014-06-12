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

module UpdateMgr.Curl where

import Data.Time.Clock
import Data.Maybe
import Control.Concurrent
import Control.Monad
import Control.Applicative
import System.Process
import System.Posix.Files
import System.Exit
import System.IO
import System.Directory
import Network.HTTP
import Tools.Misc
import UpdateMgr.Types
import Tools.Log
import Tools.Text
import Tools.IfM

type FileSize = Integer

data DownloadHandle = DownloadHandle Process FileSize URL FilePath (MVar Last)

type Process = MVar (Maybe ProcessHandle)

-- Last event seen.
data Last = Last FileSize UTCTime

data DownloadInitErr =
     CurlErr Int String
   | HttpHeaderErr
   | HttpErr (Int,Int,Int)


curlBeginDownload :: URL -> FilePath -> IO (Either DownloadInitErr DownloadHandle)
curlBeginDownload = curlProcess False

curlResumeDownload :: URL -> FilePath -> IO (Either DownloadInitErr DownloadHandle)
curlResumeDownload = curlProcess True

-- Supposed to be blocking.
curlGetNextDownloadEvent :: DownloadHandle -> IO DownloadEvent
curlGetNextDownloadEvent (DownloadHandle process fileSize_ url path last) = do
  p <- readMVar process
  case p of
    Nothing -> return DownloadCancelled
    Just handle ->
        do e <- getProcessExitCode handle
           case e of
             Just ExitSuccess -> return DownloadFinished
             -- exit code 18 means part of file was transferred (which is ok on resume)
             Just (ExitFailure c) | c == 18 -> return DownloadFinished
                                  | otherwise -> return DownloadError
             -- Still Running:
             Nothing -> modifyMVar last $ \(Last lastLength lastTime) -> do
                          waitUntil (addUTCTime 0.1 {- second -} lastTime)
                          now <- getCurrentTime
                          curLength <- fileLen path
                          return ( Last curLength now
                                 , DownloadProgressed
                                   DownloadProgress {
                                     dlComplete = float curLength / float fileSize_
                                   , dlSpeed    = float (curLength - lastLength) /
                                                  diff_time now lastTime } )
    where
      diff_time a b = realToFrac $ diffUTCTime a b
      float = fromIntegral
      fileLen p = do
           exists <- doesFileExist p
           if exists
              then fromIntegral . fileSize <$> getFileStatus p
              else return 0

      waitUntil :: UTCTime -> IO ()
      waitUntil then_ = do now <- getCurrentTime
                           let delay = round $ 10^6 * diffUTCTime then_ now
                           threadDelay (max 0 delay)

curlCancelDownload :: DownloadHandle -> IO ()
curlCancelDownload (DownloadHandle process _ _ path _) =
  modifyMVar_ process case_
  where case_ (Just processHandle) = do
          terminateProcess processHandle
          whenM (doesFileExist path) $ removeFile path
          return Nothing
        -- Already cancelled.
        case_ left = return left

curlPath = "/usr/bin/curl"

getContentLength :: URL -> IO (Either DownloadInitErr Integer)
getContentLength url = parse <$> readProcessWithExitCode curlPath ["--head", url] "" where
    parse (ExitSuccess,stdout,_) = from_resp . parseResponseHead . lines $ stdout
    parse (ExitFailure x,_, stderr) = Left (CurlErr x stderr)
    from_resp (Left _) =
        Left HttpHeaderErr
    from_resp (Right (code, resp, headers)) =
        case code of
          (2,_,_) -> maybe (Left HttpHeaderErr) Right (msum . map extract_len $ headers)
          _       -> Left $ HttpErr code
    extract_len (Header HdrContentLength s) = maybeRead s
    extract_len _ = Nothing

-- FIXME:
-- this needs to use c based fork, as xenstore seems to be borked after this
-- and lockups and hangs happen if using watches (had similar problem in
-- disksyncmgr)

curlProcess :: Bool -> URL -> FilePath -> IO (Either DownloadInitErr DownloadHandle)
curlProcess resume url path = getContentLength url >>= from_size where
    from_size (Left er)    = return $ Left er
    from_size (Right size) = do
      [nullIn,nullOut] <- mapM (openBinaryFile "/dev/null") [ReadMode, WriteMode]
      (Nothing, Nothing, Nothing, processHandle) <-
          createProcess CreateProcess {
              cmdspec = RawCommand curlPath ("-o":path:url:if resume
                                                           then ["-C","-"]
                                                           else [])
              , cwd = Nothing
              , env = Nothing
              , std_in = UseHandle nullIn
              , std_out = UseHandle nullOut
              , std_err = Inherit
              , close_fds = True }
      last <- newMVar . Last 0 =<< getCurrentTime
      process <- newMVar (Just processHandle)
      return $ Right (DownloadHandle process size url path last)
