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

{-# LANGUAGE ScopedTypeVariables #-}
module StoreHouse.VHD.Util
    ( vhdCreate
    , vhdSnapshot
    , vhdQueryAllocatedBlockCount
    , vhdQueryIsEmpty
    , vhdQueryVirtualSize
    , vhdQueryMaxVirtualSizeForFastResize
    ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Word
import qualified Data.Vhd as Vhd

import Control.Applicative
import Control.Monad
import Control.Exception

import System.IO
import System.Process
import System.Directory
import System.Exit

import Tools.Uuid
import Tools.Log

vhdCreate :: FilePath -> Word64 -> IO ()
vhdCreate path sizeMiB = do
    info $ "creating VHD (path = '" ++ show path ++ "', size = "++ show sizeMiB ++ " MiB)"
    (exitCode,_,stderr) <- readProcessWithExitCode "vhd-util"
        [ "create"
        , "-s"
        , show sizeMiB
        , "-n"
        , path
        ] ""
    case exitCode of
        ExitSuccess -> return ()
        _           -> error ("cannot create VHD : " ++ show exitCode ++ " : " ++ show stderr)

vhdSnapshot :: FilePath -> FilePath -> IO ()
vhdSnapshot parentpath path = do
    info $ "taking snapshot of VHD"
    (exitCode,_,stderr) <- readProcessWithExitCode "vhd-util"
        [ "snapshot"
        , "-p"
        , parentpath
        , "-n"
        , path
        ] ""
    case exitCode of
        ExitSuccess -> return ()
        _           -> error ("cannot snapshot VHD : " ++ show exitCode ++ " : " ++ show stderr)

vhdQueryAllocatedBlockCount :: FilePath -> IO Word64
vhdQueryAllocatedBlockCount path = do
    (exitCode, stdOut , stdErr) <- readProcessWithExitCode "vhd-util"
        [ "query", "-v", "-a", path] ""
    case exitCode of
        ExitSuccess -> return (read stdOut)
        _           -> error ("cannot query VHD : " ++ show exitCode ++ " : " ++ show stdErr)

vhdQueryIsEmpty :: FilePath -> IO Bool
vhdQueryIsEmpty path = do
    liftM (== 0) (vhdQueryAllocatedBlockCount path)

-- TODO: returns a value in MiB: encode this in the type system.
vhdQueryVirtualSize :: FilePath -> IO Word64
vhdQueryVirtualSize path = do
    (exitCode, stdOut , stdErr) <- readProcessWithExitCode "vhd-util"
        [ "query", "-v", "-n", path] ""
    case exitCode of
        ExitSuccess -> return (read stdOut)
        _           -> error ("cannot query VHD : " ++ show exitCode ++ " : " ++ show stdErr)

-- TODO: returns a value in MiB: encode this in the type system.
vhdQueryMaxVirtualSizeForFastResize :: FilePath -> IO Word64
vhdQueryMaxVirtualSizeForFastResize path = do
    (exitCode, stdOut , stdErr) <- readProcessWithExitCode "vhd-util"
        [ "query", "-S", "-n", path] ""
    case exitCode of
        ExitSuccess -> return (read stdOut)
        _           -> error ("cannot query VHD : " ++ show exitCode ++ " : " ++ show stdErr)
