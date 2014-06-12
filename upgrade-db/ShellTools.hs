--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

module ShellTools ( spawnShell
                  , spawnShell'
                  , safeSpawnShell ) where

import qualified Data.Text as T
import System.Process
import System.IO
import System.Exit
import Control.Exception (throwIO)

-- Execute shell command and wait for its output, return empty string in case of exit failure
spawnShell :: String -> IO String
spawnShell cmd =
    spawnShell' cmd >>= f where f Nothing  = return ""
                                f (Just s) = return s

-- Execute shell command and wait for its output, return Nothing on failure exit code
spawnShell' :: String -> IO (Maybe String)
spawnShell' cmd =
    runInteractiveCommand cmd >>= \ (_, stdout, _, h) ->
        do contents <- hGetContents stdout
           -- force evaluation of contents
           exitCode <- length contents `seq` waitForProcess h
           case exitCode of
             ExitSuccess -> return $ Just contents
             _           -> return   Nothing

-- Execute shell command and wait for its output, cause exception on failure exit code
safeSpawnShell :: String -> IO String
safeSpawnShell cmd =
    spawnShell' cmd >>= f where
        f Nothing  = error $ message
        f (Just s) = return s
        message    = "shell command: " ++ cmd ++ " FAILED."
