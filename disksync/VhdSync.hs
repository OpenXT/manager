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

module VhdSync
    ( SyncProcess
    , SyncEvent(..)
    , SyncStage(..)
    , SyncState(..)
    , SyncStateChangeReason(..)
    , SyncResultDetails(..)
    , SyncResultReason(..)
    , SyncAdmctlDetails(..)
    , SyncSrvcksumDetails(..)
    , SyncError(..)
    , SyncProgress(..)
    , startDownload, startUpload
    , waitSyncEvent
    , cleanupSyncProcess
    , pauseSyncProcess
    , resumeSyncProcess
    ) where

import Data.IORef
import Data.List
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import qualified Control.Exception as E
import System.FilePath
import System.IO
import qualified System.Posix.Process as P
import System.Posix.Types
import System.Posix.Signals
import System.Exit
import System.Timeout
import Tools.Log
import Tools.Misc
import Tools.VhdSyncC
import qualified Network.Socket as N

import Types

data Key = Key { keyPath :: FilePath }
data Mode = Input | Output
data Header = Header String String
data SyncOption = ZeroArgOption String
                | OneArgOption String String

data SyncProcess =
     SyncProcess {
      handle :: ProcessID
    , controlSocket :: N.Socket
    , recvBuffer :: IORef String
    , cleaningup :: IORef Bool
    , syncMode :: Mode
    , processStatus :: MVar P.ProcessStatus
    }

data SyncProgress = SyncProgress {
      progressTime :: Double
    , progressRemaining :: Double
    , progressTotal :: Double
    , progressUlSpeed :: Double
    , progressDlSpeed :: Double
    } deriving ( Eq, Show )

data XferProgress = XferProgress {
      xferTime :: Double
    , xferDlTotal :: Double
    , xferDlNow :: Double
    , xferDlSpeed :: Double
    , xferUlTotal :: Double
    , xferUlNow :: Double
    , xferUlSpeed :: Double
    } deriving ( Eq, Show )

syncProgressFromXfer :: Mode -> XferProgress -> SyncProgress
syncProgressFromXfer Output xfer = SyncProgress (xferTime xfer) (xferDlTotal xfer - xferDlNow xfer) (xferDlTotal xfer) (xferUlSpeed xfer) (xferDlSpeed xfer)
syncProgressFromXfer Input  xfer = SyncProgress (xferTime xfer) (xferUlTotal xfer - xferUlNow xfer) (xferUlTotal xfer) (xferUlSpeed xfer) (xferDlSpeed xfer)

data SyncStateChangeDetails = SyncStateChangeDetails {
      scState :: SyncState
    , scReason :: SyncStateChangeReason
    } deriving ( Eq, Show )

data SyncState = Running
               | Paused deriving ( Eq, Show, Enum )

data SyncStateChangeReason = None
                           | User
                           | NetworkInterruption
                           | AdmissionControl
                           | ServerChecksumming deriving ( Eq, Show, Enum )


type SyncResultCode = Int --0 is OK, anything else is an error

data SyncResultReason = Success              --OK
                      | FileAccessError      --Image file could not be opened (filer could have gone down, etc.)
                      | InvalidChunkParams   --Invalid parameters for chunked GET
                      | InvalidRange         --Invalid range specified for ranged GET
                      | MalformedRangeHeader --Malformed range header for ranged GET
                      | TransferSizeTooLarge --Transfer size greater than 128GB
                      | FileNotUnique        --Specified filename is not unique in the image repository
                      | MalformedURI         --Malformed request URI
                      | FileNotExists        --File does not exist in the specified image repository
                      | FileNotTracked       --File is not tracked in the backend database
                      | InsufficientSpace    --Not enough disk space in image repository to complete the transfer
                      | InvalidRpc           --Specified RPC does not map to one of our exposed services
                      | InvalidRpcMethod     --Specified RPC method does not map to one of the methods on the specified service
                      | InvalidImageState    --Image file in bad state (e.g., FINAL for upload or PARTIAL for download)
                      | DigestMalformed      --Checksum specification invalid (syntax of checksum line wrong)
                      | DigestMismatch       --Checksums on client/server don't match
                      | IncompatibleVersion  --Client version is not compatible with the server version
                      | InvalidKey           --Key validation of encrypted image failed
                      | AccessDenied         --Cannot grant access to the file (anymore)
                      | HTTPNotAllowed       --HTTP transfer requested but not HTTP not configured
                      | TooManyXfers         --Too many transfers are happening right now
                      | AdmissionError       --Couldn't talk to the admission daemon
                      | DbAccessNotAllowed   --Tried to use the DB when it was not allowed
                      | InvalidGenNumber     --Image client generation number does not match that on the server
                      | ImageCoalesced       --Image has been coalesced on the server
                      | ImageAlreadyExists   --Upload was attempted but image already exists on the server.
                      | LocalChecksumFailureDvd --Local Checksum failed on DVD
                      | LocalChecksumFailureUsb --Local Checksum failed on USB
                      | LocalChecksumNotFoundDvd  --Local Checksum not found on DVD
                      | LocalChecksumNotFoundUsb  --Local Checksum not found on USB
                      | LocalChecksumPartFailDvd  --Local Checksum Partial Failure on DVD
                      | LocalChecksumPartFailUsb  --Local Checksum Partial Failure USB
                      | LocalChecksumMalformedDvd --Local Checksum Malformed on DVD
                      | LocalChecksumMalformedUsb --Local Checksum Malformed on USB
                        deriving ( Eq, Show )

instance Enum SyncResultReason where
    toEnum 0   = Success
    toEnum 101 = FileAccessError
    toEnum 102 = InvalidChunkParams
    toEnum 103 = InvalidRange
    toEnum 104 = MalformedRangeHeader
    toEnum 105 = TransferSizeTooLarge
    toEnum 106 = FileNotUnique
    toEnum 107 = MalformedURI
    toEnum 108 = FileNotExists
    toEnum 109 = FileNotTracked
    toEnum 110 = InsufficientSpace
    toEnum 111 = InvalidRpc
    toEnum 112 = InvalidRpcMethod
    toEnum 113 = InvalidImageState
    toEnum 114 = DigestMalformed
    toEnum 115 = DigestMismatch
    toEnum 116 = IncompatibleVersion
    toEnum 117 = InvalidKey
    toEnum 118 = AccessDenied
    toEnum 119 = HTTPNotAllowed
    toEnum 120 = TooManyXfers
    toEnum 121 = AdmissionError
    toEnum 122 = DbAccessNotAllowed
    toEnum 123 = ImageCoalesced
    toEnum 124 = InvalidGenNumber
    toEnum 125 = ImageAlreadyExists
    toEnum (-1)  = LocalChecksumFailureDvd
    toEnum (-2)  = LocalChecksumFailureUsb
    toEnum (-3)  = LocalChecksumNotFoundDvd
    toEnum (-4)  = LocalChecksumNotFoundUsb
    toEnum (-5)  = LocalChecksumPartFailDvd
    toEnum (-6)  = LocalChecksumPartFailUsb
    toEnum (-7)  = LocalChecksumMalformedDvd
    toEnum (-8)  = LocalChecksumMalformedUsb
    toEnum x   = error ("unknown vhd-sync result reason " ++ show x )

    fromEnum Success                   = 0
    fromEnum FileAccessError           = 101
    fromEnum InvalidChunkParams        = 102
    fromEnum InvalidRange              = 103
    fromEnum MalformedRangeHeader      = 104
    fromEnum TransferSizeTooLarge      = 105
    fromEnum FileNotUnique             = 106
    fromEnum MalformedURI              = 107
    fromEnum FileNotExists             = 108
    fromEnum FileNotTracked            = 109
    fromEnum InsufficientSpace         = 110
    fromEnum InvalidRpc                = 111
    fromEnum InvalidRpcMethod          = 112
    fromEnum InvalidImageState         = 113
    fromEnum DigestMalformed           = 114
    fromEnum DigestMismatch            = 115
    fromEnum IncompatibleVersion       = 116
    fromEnum InvalidKey                = 117
    fromEnum AccessDenied              = 118
    fromEnum HTTPNotAllowed            = 119
    fromEnum TooManyXfers              = 120
    fromEnum AdmissionError            = 121
    fromEnum DbAccessNotAllowed        = 122
    fromEnum ImageCoalesced            = 123
    fromEnum InvalidGenNumber          = 124
    fromEnum ImageAlreadyExists        = 125
    fromEnum LocalChecksumFailureDvd   = (-1)
    fromEnum LocalChecksumFailureUsb   = (-2)
    fromEnum LocalChecksumNotFoundDvd  = (-3)
    fromEnum LocalChecksumNotFoundUsb  = (-4)
    fromEnum LocalChecksumPartFailDvd  = (-5)
    fromEnum LocalChecksumPartFailUsb  = (-6)
    fromEnum LocalChecksumMalformedDvd = (-7)
    fromEnum LocalChecksumMalformedUsb = (-8)

data SyncResultDetails = SyncResultDetails {
      srCode :: SyncResultCode
    , srReason :: SyncResultReason
    } deriving ( Eq, Show )

data SyncAdmctlDetails = SyncAdmctlDetails {
      saTime :: Double        --message time
    , saNextContact :: Double --number of seconds until next attempt to contact server
    , saQueuePosition :: Int --0-based position in the wait queue on server
} deriving ( Eq, Show )

data SyncSrvcksumDetails = SyncSrvcksumDetails {
      ckTime :: Double        --message time
    , ckNextContact :: Double --number of seconds until next attempt to contact server
} deriving ( Eq, Show )

data SyncEvent = Finished
               | Failed SyncError
               | OnDemandStop
               | StageProgress SyncStage SyncProgress
               | StateChange SyncState SyncStateChangeReason
               | Result SyncResultDetails
               | Admctl SyncAdmctlDetails
               | ServerCksum SyncSrvcksumDetails
               | ClearThroat
               | Unexpected String
                 deriving ( Eq, Show )

data SyncStage = Chksum | Install | Transfer deriving ( Eq, Show )

data SyncError = FailureExitCode Int
               | SocketError String
                 deriving ( Eq, Show )
type EventParser = String -> Maybe SyncEvent

mode_str Input  = "input"
mode_str Output = "output"

-- babysits PID and stores the waitpid result in mvar
-- can be only called once, because waitpid (aka P.getProcessStatus here) can be only called once
babysitProcessStatus :: ProcessID -> MVar P.ProcessStatus -> IO ()
babysitProcessStatus pid status_mv =
    do x <- E.try $ P.getProcessStatus True False pid
       case x of
         Left ex -> do
             warn $ "trouble waiting for PID " ++ show pid ++ ": " ++ show (ex :: E.SomeException)
             repeat
         Right Nothing ->
             repeat
         Right (Just s) ->
             putMVar status_mv s
    where
      repeat = babysitProcessStatus pid status_mv

getSyncProcessStatusNonBlocking :: SyncProcess -> IO (Maybe P.ProcessStatus)
getSyncProcessStatusNonBlocking process = do
    -- non blocking case
    s <- tryTakeMVar (processStatus process)
    case s of
      Nothing -> return Nothing -- don't have status yet
      Just s  -> putMVar (processStatus process) s >> (return . Just $ s) -- recycle the status for further reads

getSyncProcessStatusBlocking :: SyncProcess -> IO P.ProcessStatus
getSyncProcessStatusBlocking process =
    -- blocking case we just try to grab mvar
    readMVar (processStatus process)

getSyncProcessStatusTimeoutSecs :: Int -> SyncProcess -> IO (Maybe ExitCode)
getSyncProcessStatusTimeoutSecs t process
    | t <= 0    = return Nothing
    | otherwise = when_code =<< getSyncProcessStatusNonBlocking process
    where
      when_code Nothing  = threadDelay (10^6) >> getSyncProcessStatusTimeoutSecs (t-1) process
      when_code (Just c) = return $ Just (fromProcessStatus c)

terminateProcess :: ProcessID -> IO ()
terminateProcess pid =
    signalProcess sigTERM pid
         `E.catch`
         ( \ex -> warn $ "trouble sending sigTERM to sync: " ++ show (ex :: E.SomeException) )


fromProcessStatus :: P.ProcessStatus -> ExitCode
fromProcessStatus (P.Exited c)     = c
fromProcessStatus (P.Terminated s) = ExitFailure $ fromIntegral s + 128
fromProcessStatus (P.Stopped s)    = ExitFailure $ fromIntegral s + 128

makeHeaders :: TransferCtxID -> [Header]
makeHeaders (TransferCtxID ctx_id) =
    [ Header "X-XCBE-Xfer-Context" ctx_id ]

makeFlags :: [(Bool, String)] -> [SyncOption]
makeFlags optsMap = map (\x -> ZeroArgOption (snd x)) $ filter (\x -> fst x) optsMap

makeIntArgs :: [(Int, String)] -> [SyncOption]
makeIntArgs optsMap = map (\(x,y) -> OneArgOption y (show x)) optsMap

makeCommonOptions :: CommonTransferOptions -> [SyncOption]
makeCommonOptions CommonTransferOptions {chunkSize=cksize, logLevel=loglevel, verbose=verbose, lowspeedLimit=lowspeed, lowspeedTime=lowtime, connectTimeout=ctimeout} = makeFlags flagsMap ++ makeIntArgs argsMap
    where
      flagsMap = [(verbose, "--verbose")]
      argsMap  = [(cksize, "--cksize"),
                  (lowspeed, "--lowspeed"),
                  (lowtime, "--lowtime"),
                  (ctimeout, "--ctimeout"),
                  (loglevel, "--loglevel")]

makeOptions :: TransferOptions -> [SyncOption]
makeOptions UploadOptions {enableCompaction=compact, commonOptions=opts} = makeFlags flagsMap ++ makeCommonOptions opts
    where
      flagsMap = [(compact, "--compact")]

makeOptions DownloadOptions {encryptLocalImage=encrypt, enableDvdCache=dvdcache, enableUsbCache=usbcache, forceLocalCheckSum=forcelocalchecksum, commonOptions=opts} = makeFlags flagsMap ++ makeCommonOptions opts
    where
      flagsMap = [(encrypt, "--encrypt-local-vhd"),
                  (dvdcache, "--enabledvd"),
                  (usbcache, "--enableusb"),
                  (forcelocalchecksum, "--force-local-checksum") ]

startDownload :: TransferOptions -> FilePath -> String -> String -> CryptoSpec -> TransferCtxID -> IO SyncProcess
startDownload dl_opts output_file transfer_url rpc_url crypto id =
    do info $ "Downloading " ++ output_file ++ " from " ++ transfer_url
       let headers = makeHeaders id
       let opts = makeOptions dl_opts
       run output_file transfer_url rpc_url Output headers crypto opts

startUpload :: TransferOptions -> FilePath -> String -> String -> CryptoSpec -> TransferCtxID -> IO SyncProcess
startUpload ul_opts input_file transfer_url rpc_url crypto id =
    do info $ "Uploading " ++ input_file ++ " to " ++ transfer_url
       let headers = makeHeaders id
       let opts = makeOptions ul_opts
       run input_file transfer_url rpc_url Input headers crypto opts

runVhdSync :: Int -> [String] -> IO ProcessID
runVhdSync monitorFd args =
    forkCloseAndExec [0,1,2,monitorFd] ("/usr/bin/vhd-sync.compress" : args)

run :: FilePath -> String -> String -> Mode -> [Header] -> CryptoSpec -> [SyncOption] -> IO SyncProcess
run file transfer_url rpc_url mode headers crypto options =
    do ( parent, child ) <- N.socketPair N.AF_UNIX N.Stream N.defaultProtocol
       info $ "created socket pair " ++ show parent ++ " " ++ show child
       info $ "calling vhd-sync with arguments: " ++ (concat . intersperse " " $ args child)
       h <- runVhdSync (fromIntegral $ N.fdSocket child) (args child)
       E.try $ N.sClose child  :: IO (Either E.SomeException ())
       buf <- newIORef ""
       cup <- newIORef False
       status_mv <- newEmptyMVar
       let process = SyncProcess { controlSocket = parent
                                 , handle = h
                                 , recvBuffer = buf
                                 , cleaningup = cup
                                 , syncMode = mode
                                 , processStatus = status_mv }
       -- run waitpid in separate thread
       forkOS . liftIO $ babysitProcessStatus h status_mv
       return process
    where
      args socket
           = [ "--" ++ (mode_str mode), file
             , "--sparse"
             , "--monitorfd", show (N.fdSocket socket)
             , "--progress"
             , "--forceurl"
             , "--cacert", cryptoServerCertPath crypto
             , "--cert", cryptoClientCertPath crypto
             , "--key", cryptoClientKeyPath crypto
             , "--transferurl", transfer_url ]
             ++ ( concat . map option_arg $ options )
             ++ ( concat . map header_arg $ headers )
             ++ [ rpc_url ]
             where
               option_arg (ZeroArgOption v)  = [v]
               option_arg (OneArgOption k v) = [k, v]
               header_arg (Header k v) = ["--header", k++":"++v]

-- wait for an event from the currently running sync process
-- if the sync process has exited, return value will be one of: Finished, OnDamandStop, or Failed
-- and we will have cleaned up the sync process
waitSyncEvent :: SyncProcess -> IO SyncEvent
waitSyncEvent process =
    when_code =<< getSyncProcessStatusNonBlocking process
    where
      when_code Nothing  = wait_for_event
      when_code (Just c) = exit_event c

      exit_event es =
          do cup <- readIORef (cleaningup process)
             if cup
                then do return OnDemandStop
                else do cleanupSyncSocket process
                        case fromProcessStatus es of
                          ExitSuccess -> return Finished
                          ExitFailure v -> return $ Failed (FailureExitCode v)

      wait_for_event =
          do r <- buffered_event
             case r of
               Nothing -> socket_event
               Just e  -> return e

      sock = controlSocket process
      quantum = 10^6 -- 1s

      socket_event =
          do r <- timeout quantum (threadWaitRead . fromIntegral . N.fdSocket $ sock)
             case r of
               Nothing -> waitSyncEvent process -- timeout
               _       -> read_parse_event

      buffered_event :: IO (Maybe SyncEvent)
      buffered_event =
          cutLine (recvBuffer process) >>= return . parse
          where
            parse Nothing  = Nothing
            parse (Just l) = Just $ parse_line l

      -- read from control socket then
      read_parse_event =
          do dat <- E.try (N.recv sock 128)
             case dat of
               -- if there was a problem reading from socket,
               -- try to terminate vhd sync, check the exit code to see whether this
               -- was just a graceful exit because task finished and vhd sync closed socket on its end and then quitted
               Left ( ex :: E.SomeException ) ->
                   do warn $ "exception: " ++ show ex
                      cup <- readIORef (cleaningup process)
                      if cup
                         then do
                           warn $ "but it is in cleanup sequence, waiting for cleanup to finish..."
                           getSyncProcessStatusBlocking process
                           warn $ "wait done."
                           return OnDemandStop
                         else do
                           status <- cleanupSyncProcess process
                           case status of
                             Exited ExitSuccess -> return Finished
                             _ -> return $ Failed (SocketError $ show ex)

               -- if read succeded, parse event
               Right dat ->
                   do modifyIORef (recvBuffer process) $ \buf -> buf ++ dat
                      cmd <- buffered_event
                      case cmd of
                        Nothing  -> waitSyncEvent process
                        Just evt -> return evt

      parse_line :: String -> SyncEvent
      parse_line l =
          case find_parser l of
            Nothing      -> Unexpected l
            Just parse   -> case parse l of
                              Nothing -> Unexpected l
                              Just ev -> ev

      find_parser :: String -> Maybe EventParser
      find_parser line =
          foldl' look Nothing parsers
          where
            look v@(Just _)  _  = v
            look Nothing     p  =
                case p of
                  (prefix,parser) | prefix `isPrefixOf` line -> Just parser
                  _                                          -> Nothing
            parsers :: [ (String,EventParser) ]
            parsers =
                [ ("cksum_progress"   , chksum_pr)
                , ("install_progress" , install_pr)
                , ("xfer_progress"    , xfer_pr)
                , ("state_change"     , state_pr)
                , ("result"           , result_pr)
                , ("admctl"           , admctl_pr)
                , ("server_checksumming", srvcksum_pr)
                , ("clear throat"     , clearthroat_pr)
                ]
                where
                  chksum_pr str =
                      do x <- parseProgress . skipWord $ str
                         return $ StageProgress Chksum x
                  install_pr str =
                      do x <- parseProgress . skipWord $ str
                         return $ StageProgress Install x
                  xfer_pr str =
                      do x <- parseXferProgress . skipWord $ str
                         return $ StageProgress Transfer (syncProgressFromXfer mode x)
                  state_pr str =
                      do x <- parseStateChange . skipWord $ str
                         return $ StateChange (scState x) (scReason x)
                  result_pr str =
                      do x <- parseResult . skipWord $ str
                         return $ Result x
                  admctl_pr str =
                      do x <- parseAdmctl . skipWord $ str
                         return $ Admctl x
                  srvcksum_pr str =
		      do x <- parseServerCksum . skipWord $ str
			 return $ ServerCksum x
                  clearthroat_pr str =
                      return ClearThroat

      mode = syncMode process

data SyncExit = Exited ExitCode
              | BrutallyKilled


cleanupSyncSocket :: SyncProcess -> IO ()
cleanupSyncSocket process =
    do -- set cleanup flag, blow the sockets, ignore errors
       writeIORef (cleaningup process) True
       info "closing control socket"
       N.sClose (controlSocket process)
            `E.catch`
            ( \ex ->
                  warn $ "trouble closing control socket: " ++ show (ex :: E.SomeException) )

cleanupSyncProcess :: SyncProcess -> IO SyncExit
cleanupSyncProcess process =
    do cleanupSyncSocket process
       info "waiting for vhd sync to exit gracefully.."
       -- give it 5s benefit of doubt
       done <- getSyncProcessStatusTimeoutSecs 5 process
       case done of
         (Just excode) -> do
                   info $ "vhd sync exited with: " ++ show excode
                   return $ Exited excode
         Nothing -> do
                   info "killing vhd sync"
                   terminateProcess (handle process)
                   done_again <- getSyncProcessStatusTimeoutSecs 5 process
                   case done_again of
                     Just _  -> info "killed"
                     Nothing -> warn "failed to kill vhd sync!"
                   return BrutallyKilled

-- Send a state change command to vhd-sync
-- log errors, but otherwise happily ignore
-- defer to waitSyncEvent for error handling
syncStateChange :: SyncProcess -> SyncState -> IO ()
syncStateChange process state =
    do dat <- E.try (N.send sock cmd)
       case dat of
         Left ( ex :: E.SomeException ) ->
             warn $ "exception: " ++ show ex
         Right sent ->
             when ( sent /= length cmd ) $ warn msg
             where
               msg = "only " ++ show sent ++ " bytes sent; tried to send " ++ show (length cmd)
    where
      sock = controlSocket process
      cmd = "state_change " ++ (show $ fromEnum state) ++ "\n"

pauseSyncProcess :: SyncProcess -> IO ()
pauseSyncProcess process = syncStateChange process Paused

resumeSyncProcess :: SyncProcess -> IO ()
resumeSyncProcess process = syncStateChange process Running

parseInt :: String -> Maybe Int
parseInt str =
    case reads str of
      ((v,_):_) -> Just v
      _         -> Nothing

parseFloat :: String -> Maybe Double
parseFloat str =
    case reads str of
      ((v,_):_) -> Just v
      _         -> Nothing

skipWord :: String -> String
skipWord s =
    case words s of
      (_:ws) -> concat . intersperse " " $ ws
      _ -> s

parseProgress :: String -> Maybe SyncProgress
parseProgress str =
    case words str of
      [time_str,rem_str,total_str] ->
          do time  <- parseFloat time_str
             rem   <- parseFloat rem_str
             total <- parseFloat total_str
             return $ SyncProgress time rem total 0 0
      _ -> Nothing

parseXferProgress :: String -> Maybe XferProgress
parseXferProgress str =
    case words str of
      [time_str, dl_tot_str, dl_now_str, dl_speed_str, ul_tot_str, ul_now_str, ul_speed_str] ->
          do time <- parseFloat time_str
             dl_tot <- parseFloat dl_tot_str
             dl_now <- parseFloat dl_now_str
             dl_speed <- parseFloat dl_speed_str
             ul_tot <- parseFloat ul_tot_str
             ul_now <- parseFloat ul_now_str
             ul_speed <- parseFloat ul_speed_str
             return $ XferProgress time dl_tot dl_now dl_speed ul_tot ul_now ul_speed
      _ -> Nothing

parseStateChange :: String -> Maybe SyncStateChangeDetails
parseStateChange str =
    case words str of
      [state_str,reason_str] ->
          do state  <- parseInt state_str
             reason <- parseInt reason_str
             return $ SyncStateChangeDetails (toEnum state) (toEnum reason)
      _ -> Nothing

-- result messages have the format "result RESULT_CODE ERROR_CODE RESULT_MSG", for example:
-- "result 0 0 Success."
-- "result 4 110 Transfer failed."
parseResult :: String -> Maybe SyncResultDetails
parseResult str =
    case (take 2 $ words str) of
      [result_str,reason_str] ->
          do result  <- parseInt result_str
             reason <- parseInt reason_str
             return $ SyncResultDetails (toEnum result) (toEnum reason)
      _ -> Nothing

parseAdmctl :: String -> Maybe SyncAdmctlDetails
parseAdmctl str =
    case words str of
      [time_str,nextcontact_str,qpos_str] ->
          do time  <- parseFloat time_str
             nextcontact <- parseFloat nextcontact_str
             qpos <- parseFloat qpos_str
             return $ SyncAdmctlDetails time nextcontact (truncate qpos)
      _ -> Nothing

parseServerCksum :: String -> Maybe SyncSrvcksumDetails
parseServerCksum str =
    case words str of
      [time_str,nextcontact_str] ->
          do time  <- parseFloat time_str
             nextcontact <- parseFloat nextcontact_str
             return $ SyncSrvcksumDetails time nextcontact
      _ -> Nothing

-- try to cut a line out of a character buffer
cutLine :: (IORef String) -> IO (Maybe String)
cutLine buf =
    do str <- readIORef buf
       case cut "" str of
         Nothing -> return Nothing
         Just (line,rest) -> do writeIORef buf rest
                                return . Just $ chomp line
    where
      cut accum [] = Nothing
      cut accum ('\n':xs) = Just (reverse accum,xs)
      cut accum (x:xs) = cut (x:accum) xs
