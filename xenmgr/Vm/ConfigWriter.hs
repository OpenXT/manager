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

{-# LANGUAGE ScopedTypeVariables #-}

module Vm.ConfigWriter ( writeXenvmConfig ) where

import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad
import Data.String
import System.FilePath
import System.IO
import XenMgr.Rpc
import Vm.Types
import Vm.Queries
import Vm.Config
import XenMgr.Config
import XenMgr.Connect.Xenvm
import Tools.Log

-- Write a XENVM configuration file for VM
writeXenvmConfig :: VmConfig -> Rpc ()
writeXenvmConfig cfg = do
  config <- getXenvmConfig cfg
  let stringconfig = stringifyXenvmConfig config

  liftIO $ retry 10 (writeFile xenvmConfigPath stringconfig)
  info $ "written xenvm config for " ++ show (vmcfgUuid cfg)
  where
    xenvmConfigDir  = "/tmp"
    xenvmConfigPath = joinPath [xenvmConfigDir, "xenmgr-xenvm-" ++ (show $ vmcfgUuid cfg)]
    retry 0 _ = return ()
    retry n f = f `E.catch` \(err::IOError) -> threadDelay (10^5) >> retry (n-1) f
