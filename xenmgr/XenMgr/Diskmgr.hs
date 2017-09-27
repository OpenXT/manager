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

module XenMgr.Diskmgr ( createVhd ) where

import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad
import Control.Monad.Trans

import System.IO
import Directory

import Tools.Process
import Tools.Log

import Vm.Types
import Vm.Uuid

-- returns VHD path
createVhd :: Int -> IO FilePath
createVhd sizeMB =
    do info $ "creating VHD (" ++ show sizeMB ++ "MB)"
       uuid <- uuidGen
       readProcessOrDie "vhd-util" [ "create", "-s", show sizeMB, "-n", "/storage/disks/" ++ show uuid ++ ".vhd" ] ""
       return $ "/storage/disks/" ++ show uuid ++ ".vhd"
