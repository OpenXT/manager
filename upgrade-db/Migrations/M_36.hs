--
-- Copyright (c) 2018 Assured Information Security, Inc.
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

{-# LANGUAGE ForeignFunctionInterface #-}

-- description: Pyro upgrade.
-- Update sshd & sshd-v4v persistent configuration files.

module Migrations.M_36 (migration) where

import UpgradeEngine
import Control.Monad
import System.Directory

migration = Migration {
              sourceVersion = 36
            , targetVersion = 37
            , actions = act
            }

upgradeSshdPersistentConfig :: IO ()
upgradeSshdPersistentConfig = do
    sshEnabled <- doesFileExist "/config/etc/ssh/enabled"
    when sshEnabled $ do
        removeFile "/config/etc/ssh/enabled"
        writeFile "/config/etc/ssh/sshd_not_to_be_run" ""

    v4vEnabled <- doesFileExist "/config/etc/ssh/disable-v4v"
    when v4vEnabled $ do
        removeFile "/config/etc/ssh/disable-v4v"
        writeFile "/config/etc/ssh/v4v_not_to_be_run" ""

act :: IO ()
act = do
    upgradeSshdPersistentConfig
