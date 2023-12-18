--
-- Copyright (c) 2014 Citrix Systems, Inc.
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

{-# LANGUAGE PatternGuards #-}
-- created 11.02.2014
module Migrations.M_21 (migration) where

import UpgradeEngine
import Data.List (foldl')
import ShellTools
import Control.Monad
import System.Directory

migration = Migration {
              sourceVersion = 21
            , targetVersion = 22
            , actions = act
            }

act :: IO ()
act = updateSyncvm >> updateSsh

updateSyncvm = xformVmJSON xform where
  xform tree = case jsGet "/type" tree of
    Just s | jsUnboxString s == "syncvm" -> modify tree
    _ -> tree
    where
      modify = jsRm "/config/kernel"

updateSsh = do
  e <- doesDirectoryExist "/config/ssh"
  when e $ mapM_ safeSpawnShell ["mv /config/ssh /config/etc/", "restorecon -R /config/etc/ssh"]
