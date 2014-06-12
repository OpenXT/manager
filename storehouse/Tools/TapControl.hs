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

module Tools.TapControl
	(
	-- Types:
		  Arguments
		, Force
		, DeviceMajorNumber
		, DeviceMinorNumber
		, ProcessId
		, DiskType (..)
		, DiskMode (..)

	-- Paired functions:
		, allocate, free
		, attach  , detach
		, create  , destroy
		, open    , close
		, pause   , unpause

	-- Other functions:
		, list
		, major
		, spawn
	)
	where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import qualified Control.Exception as C
import Control.Monad

import Data.Bits
import Data.Ratio
import Data.Word
import System.Exit
import System.Environment
import System.Posix.Files
import System.Posix.Types
import System.Process
import System.Process.Internals
import System.IO
import Tools.Log

type Arguments         = String
type Force             = Bool
type DeviceMajorNumber = Word16
type DeviceMinorNumber = Word32
type ProcessId         = Word64

data State
	= StateClosed
	| StatePaused
	| StateRunning
	| StateUnknown Word32
	deriving Eq

instance Show State where
	show  StateClosed     = "closed"
	show  StatePaused     = "paused"
	show  StateRunning    = "running"
	show (StateUnknown i) = "unknown (" ++ show i ++ ")"

data DiskMode
	= DiskModeReadOnly
	| DiskModeReadWrite
	deriving Eq

instance Show DiskMode where
	show DiskModeReadOnly  = "ro"
	show DiskModeReadWrite = "rw"

type DiskPath = FilePath

data DiskType = DiskTypeVhd deriving Eq

instance Show DiskType where
	show DiskTypeVhd = "vhd"

stateFromInteger 0x00 = StateRunning
stateFromInteger 0x02 = StateClosed
stateFromInteger 0x2a = StatePaused
stateFromInteger    x = StateUnknown x

readProcessWithEnvAndExitCode cmd addEnv args input = do
    newEnv <- if null addEnv
                  then return Nothing
                  else do allEnv <- getEnvironment
                          return $ Just (allEnv ++ addEnv)
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe,
                                       env     = newEnv }

    outMVar <- newEmptyMVar
    out  <- hGetContents outh
    forkIO $ C.evaluate (length out) >> putMVar outMVar ()
    err  <- hGetContents errh
    forkIO $ C.evaluate (length err) >> putMVar outMVar ()
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    ex <- waitForProcess pid
    return (ex, out, err)

-- | Calls the command at file system path /p/ with the specified arguments.
call :: FilePath -> [(String,String)] -> [String] -> IO String
call p envs arguments = do
	info $ "calling " ++ p ++ " with arguments " ++ show arguments
	(exitCode, stdOut, stdErr) <- readProcessWithEnvAndExitCode p envs arguments ""
	case exitCode of
		ExitSuccess   -> return stdOut
		ExitFailure _ -> error $
			"error while calling "
				++ p
				++ show arguments ++ " : "
				++ show exitCode  ++ " : "
				++ stdErr

-- | Calls the "tap-ctl" command with the given function name and arguments.
callTap :: String -> [String] -> IO String
callTap function arguments = call "tap-ctl" [] (function : arguments)

-- | Just like callTap but also provides a way to augment the environment variables.
callTapEnv function env arguments = call "tap-ctl" env (function : arguments)

ignore x = ()

-- | Splits device identifier /i/ into its major and minor component, assuming
--   that identifier /i/ is of the format 0xmmmMMMmm.
deviceMajorMinor :: DeviceID -> (DeviceMajorNumber, DeviceMinorNumber)
deviceMajorMinor i = (fromInteger major, fromInteger minor) where
	major = (j .&. 0x000fff00) `shiftR`  8
	minor = (j .&. 0xfff00000) `shiftR` 12 .|.
	        (j .&. 0x000000ff)
	j = numerator $ toRational i

-- | Allocates a device. Returns the minor number of the newly-allocated device.
allocate :: IO DeviceMinorNumber
allocate = do
	filePath <- callTap "allocate" []
	fileStatus <- getFileStatus filePath
	return $ snd $ deviceMajorMinor $ specialDeviceID fileStatus

-- | Frees the device with minor number /m/.
free :: DeviceMinorNumber -> IO ()
free m = fmap ignore $ callTap "free" ["-m", show m]

-- | Attaches process /p/ to the device with minor number /m/.
attach :: ProcessId -> DeviceMinorNumber -> IO ()
attach p m = fmap ignore $ callTap "attach" ["-p", show p, "-m", show m]

-- | Detaches process /p/ from the device with minor number /m/, and then
--   kills process /p/.
detach :: ProcessId -> DeviceMinorNumber -> IO ()
detach p m = fmap ignore $ callTap "detach" ["-p", show p, "-m", show m]

-- | Pre-creates and attaches a process-device pair, then uses it to open
--   the virtual disk image at file path /f/ of type /t/ with access mode
--   /r/. Returns a tuple with the path to the newly-allocated device and
--   the minor number of that device.
--
--   Calling 'create' /f/ /t/ /r/ is equivalent to performing the sequence:
--
--       1. /p/ <- 'spawn'
--
--       2. /m/ <- 'allocate'
--
--       3. 'attach' /p/ /m/
--
--       4. 'open' /p/ /m/ /f/ /t/ /r/
--
create :: FilePath -> DiskType -> DiskMode -> Maybe FilePath -> IO (FilePath, DeviceMinorNumber)
create f t r cryptoKeyDir = do
	let addEnv = maybe [] (\k -> [("TAPDISK2_CRYPTO_KEYDIR", k)]) cryptoKeyDir
	tapDevicePathUntrimmed <- callTapEnv "create" [] (argumentsMain ++ argumentsReadOnly)
	let tapDevicePath = head $ lines tapDevicePathUntrimmed
	tapDeviceStatus <- getFileStatus tapDevicePath
	return (tapDevicePath, snd $ deviceMajorMinor $ specialDeviceID tapDeviceStatus)
	where
		argumentsMain     = ["-a", show t ++ ":" ++ f]
		argumentsReadOnly = if r == DiskModeReadOnly then ["-R"] else []

-- | Closes the virtual disk image currently opened with process /p/ and
--   device with minor number /m/, then kills process /p/ and frees the
--   device with minor number /m/.
--
--   Calling 'destroy' /p/ /m/ is equivalent to peforming the sequence:
--
--       1. 'close' /p/ /m/
--
--       2. 'detach' /p/ /m/
--
--       3. 'free' /m/
--
destroy :: Maybe ProcessId -> DeviceMinorNumber -> IO ()
destroy p m = fmap ignore $ callTap "destroy" args
	where args = maybe [] (\pid -> ["-p", show pid]) p
		++ ["-m", show m]

-- | Uses process /p/ attached to device with minor number /m/ to open the
--   virtual disk image at file path /f/ of type /t/ with access mode /r/.
open :: ProcessId -> DeviceMinorNumber -> FilePath -> DiskType -> DiskMode -> IO ()
open p m f t r = fmap ignore $
	callTap "create" (argumentsMain ++ argumentsReadOnly)
	where
		argumentsMain     = ["-p", show p, "-m", show m, "-a", show t ++ ":" ++ f]
		argumentsReadOnly = if r == DiskModeReadOnly then ["-R"] else []

-- | Closes the virtual disk image currently opened with process /p/ and
--   device with minor number /m/.
close :: ProcessId -> DeviceMinorNumber -> Force -> IO ()
close p m force = fmap ignore $
	callTap "close" (argumentsMain ++ argumentForce) where
		argumentsMain = [ "-p", show p, "-m", show m]
		argumentForce = if force then ["-f"] else []

-- | Pauses the device with minor number /m/.
pause :: DeviceMinorNumber -> IO ()
pause m = fmap ignore $ callTap "pause" ["-m", show m]

-- | Unpauses the device with minor number /m/.
unpause :: DeviceMinorNumber -> IO ()
unpause m = fmap ignore $ callTap "unpause" ["-m", show m]

type ListEntry = (Maybe ProcessId, Maybe DeviceMinorNumber, Maybe State, Maybe Arguments)

-- | Lists all tapdisk processes and devices, along with their states.
list :: IO [ListEntry]
list = fmap extractLines $ callTap "list" [] where
	extractLines :: String -> [ListEntry]
	extractLines x = map extractLine $ lines x
	extractLine :: String -> ListEntry
	extractLine x =
		( extract "pid"
		, extract "minor"
		, fmap stateFromInteger $ extract "state"
		, extract "args" )
		where
			pairs = map extractKeyValuePair $ words x
			extract s = fmap read $ lookup s pairs
	extractKeyValuePair :: String -> (String, String)
	extractKeyValuePair x = (takeWhile ne x, drop 1 $ dropWhile ne x) where ne = (/= '=')

-- | Returns the major number of all tapdisk devices.
major :: IO DeviceMajorNumber
major = fmap read $ callTap "major" []

-- | Spawns a new tapdisk process. Returns the process identifier of the new process.
spawn :: IO ProcessId
spawn = fmap read $ callTap "spawn" []

