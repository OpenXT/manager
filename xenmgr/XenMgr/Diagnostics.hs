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

module XenMgr.Diagnostics ( diagSave
                          , diagGather
                          , diagStatusReport
                          , diagStatusReportScreen
                          , diagTaasAuthenticateCredentials
                          , diagTaasUpload
                          , diagTaasAgreeTerms
                          ) where

import System.IO
import System.FilePath
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString
import Data.Word
import Data.Char
import Data.List
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Directory
import Text.Printf
import XenMgr.Rpc
import Tools.XenStore
import Tools.Misc
import Tools.Log
import Tools.Text
import Tools.Process
import XenMgr.Errors
import XenMgr.Notify
import XenMgr.Config
import Vm.Queries
import System.Process
import System.Directory (canonicalizePath)
import System.Posix.Files
import System.Exit
import Vm.Uuid

activeGuests = getGuestVms >>= filterM isRunning
activeGuestsWithPvAddons = activeGuests >>= filterM getVmPvAddons

diagSave :: String -> Rpc FilePath
diagSave mode = do
    guests <- activeGuests
    guestsWithAddons <- activeGuestsWithPvAddons
    uuid <- liftIO uuidGen
    let dir = "/tmp/sr-xcdiag-" ++ uuidStr uuid
    liftIO (createDirectory dir)
    when (not . null $ guests) $ do
      info $ "Gathering domain diagnostic info to " ++ dir
      liftIO $ do xsWrite "/xenmgr/diag-dir" dir
                  xsWrite "/xenmgr/diag-count" "0"
      notifyDiagGatherRequest mode
      timeoutSecs <- appXcDiagTimeout
      liftIO $ do spin_and_wait timeoutSecs (length guestsWithAddons)
                  xsRm "/xenmgr/diag-dir"
                  xsRm "/xenmgr/diag-count"
      info $ "Finished gathering domain diagnostic info"
    return dir  
    where
      chomp = reverse . dropWhile (\x -> x `elem` ['\r', '\n']) . reverse
      spin_and_wait 0 _ = return ()
      spin_and_wait timeout expectedCount =
          do count <- readCount
             case count of
               Just x | x >= expectedCount -> return () -- terminate early
               _                           -> threadDelay (10^6) >> spin_and_wait (timeout-1) expectedCount

decodeBytes :: String -> [Word8]
decodeBytes [] = []
decodeBytes (a:b:xs) = fromIntegral ((16*dec a) + (dec b)) : decodeBytes xs where
    dec ch | ch >= '0' && ch <= '9' = ord ch - ord '0'
           | ch >= 'a' && ch <= 'f' = ord ch - ord 'a' + 10
           | ch >= 'A' && ch <= 'F' = ord ch - ord 'A' + 10
           | otherwise              = error $ "unexpected char: " ++ show ch
decodeBytes _ = error $ "malformed bytes string"

readCount :: IO (Maybe Int)
readCount = xsRead "/xenmgr/diag-count" >>= return . (fmap read)

incrementCount :: IO ()
incrementCount = readCount >>= return . fmap (+1) >>= write where
    write (Just x) = xsWrite "/xenmgr/diag-count" . show $ x
    write _        = return ()

diagGather :: String -> String -> Rpc ()
diagGather name bufferStr = do
    let buffer = decodeBytes bufferStr
    maybe_dir <- liftIO $ xsRead "/xenmgr/diag-dir"
    case maybe_dir of
      Nothing  -> failNotGatheringDiagnostics
      Just dir -> do let bytes = B.pack buffer
                     let fname = dir `combine` name
                     info $ "gather request to " ++ show name ++ " target=" ++ show fname ++ " file=" ++ show (takeFileName fname)
                     when (not $ dir `isPrefixOf` fname) $ error "bad file name"
                     when (".." `elem` splitDirectories fname) $ error "bad file name"
                     when (takeFileName fname /= name) $ error "bad file name"
                     info $ "Writing diagnostics file " ++ show fname
                     liftIO $ do B.writeFile fname bytes
                                 incrementCount

diagStatusReport :: Bool -> Bool -> String -> String -> String -> String -> IO FilePath
diagStatusReport screenshots guest_info summary description repro_steps ticket =
    do info $ "creating status report..."
       Data.ByteString.writeFile user_report_path (UTF8.fromString user_report_xml)
       tarball <- chomp <$> safeSpawnShell (printf "status-tool %s 2>/dev/null" opts)
       removeFile user_report_path
       return tarball

    where
      user_report_path = "/tmp/user-report.xml"
      user_report_xml =
          "<user-report>\n"
          ++ "<report-version>\n1.0\n</report-version>\n"
          ++ summary_xml
          ++ description_xml
          ++ steps_xml
          ++ ticket_xml
          ++ "</user-report>\n"
      summary_xml = printf "<summary>\n%s\n</summary>\n" summary
      description_xml = printf "<description>\n%s\n</description>\n" description
      steps_xml = printf "<repro-steps>\n<step index=1>\n%s\n</step>\n</repro-steps>\n" repro_steps
      ticket_xml = printf "<support-ticket>\n%s\n</support-ticket>\n" ticket

      opts = concat . intersperse " " $ [ opt_screenshots, opt_guest_info, "--quiet", "--print-tarball" ]
      opt_screenshots | screenshots = "--screenshots"
                      | otherwise   = "--no-screenshots"
      opt_guest_info  | guest_info  = "--vm-diagnostics"
                      | otherwise   = "--no-vm-diagnostics"

-- TODO: decouple that from graphics fallback vm?
diagStatusReportScreen :: Bool -> Rpc ()
diagStatusReportScreen visible =
    do info $ "status-report-screen " ++ show visible
       uuid <- getGraphicsFallbackVm
       case uuid of
         Nothing   -> error "no graphics fallback vm"
         Just uuid -> do domid_ <- getDomainID uuid
                         case domid_ of
                           Just domid -> liftIO $ xsWrite ("/local/domain/" ++ show domid ++ "/report/state") state
                           _ -> error "no graphics fallback domain"
    where state | visible = "1"
                | otherwise = "0"

diagTaasAuthenticateCredentials :: String -> String -> Rpc [String]
diagTaasAuthenticateCredentials username password =
    do info  "Authenticating MyCitrite credentials . . . "
       (exitCode,stdOutput,_) <- liftIO $ readProcessWithExitCode_closeFds "status-upload" ["-u", username, "-p", password, "-o", "getlegal" ] ""
       if (exitCode == ExitSuccess)
           then do info "Authentication successful"
                   return [stdOutput]
           else do
             case lines stdOutput of
               (line:_) ->
                 case reverse (words line) of
                   ("connection.":_)  -> failNetworkDisconnected
                   ("credentials.":_) -> failInvalidCredentials
                   _                 -> error $ "unexpected error"
               _       -> error $ "unexpected error"

diagTaasUpload :: String -> String -> String -> String -> Rpc Bool
diagTaasUpload username password caseid filename =
    do info $ " Uploading file " ++ show filename ++ ". . ."
       (exitCode,stdOutput,_) <- liftIO $ readProcessWithExitCode_closeFds "status-upload" ["-u", username, "-p", password, "-c", caseid, "-o", "upload", filename ] ""
       if (exitCode == ExitSuccess)
           then do info "File successfully uploaded"
                   return True
           else do
             case lines stdOutput of
               (line:_) ->
                 case reverse (words line) of
                   ("connection.":_)  -> failNetworkDisconnected
                   ("exist":_)       -> failNoSuchFile
                   ("ID.":_)          -> failInvalidCaseId
                   ("credentials.":_) -> failInvalidCredentials
                   _                 -> error $ "unexpected error"
               _       -> error $ "unexpected error"

diagTaasAgreeTerms :: String -> String -> String -> Rpc Bool
diagTaasAgreeTerms username password agreed_version =
    do info "Checking latest version with agreed version . . . "
       (exitCode,stdOutput,_) <- liftIO $ readProcessWithExitCode_closeFds "status-upload" ["-u", username, "-p", password, "-o", "getlegal" ] ""
       let version_no = drop 9 (lines stdOutput !! 1)
       if (exitCode /= ExitSuccess)
           then do
             case lines stdOutput of
               (line:_) ->
                 case reverse (words line) of
                   ("connection.":_)  -> failNetworkDisconnected
                   ("credentials.":_) -> failInvalidCredentials
                   _                 -> error $ "unexpected error"
               _       -> error $ "unexpected error"
            else if (show version_no == show agreed_version)
                    then do info "User has agreed to the latest TaaS terms"
                            return True
                    else failTaasTerms
