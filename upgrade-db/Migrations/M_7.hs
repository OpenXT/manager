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

{-# LANGUAGE PatternGuards #-}
module Migrations.M_7 (migration) where

import Control.Monad
import Directory
import Data.List
import System.Posix.Files

import UpgradeEngine
import ShellTools

migration = Migration {
              sourceVersion = 7
            , targetVersion = 8
            , actions = act
            }

act :: IO ()
act = do
  v4vRules
  extSSH

v4vRules = xformVmJSON xform where
    xform tree
        | Just t <- typ, t `elem` ["svm"] = add_rules tree
        | otherwise                       = tree
        where
          typ = jsUnboxString `fmap` (jsGet "/type" tree)

    add_rules tree = foldl' (\t (p,v) -> jsSet p (jsBoxString v) t) (jsRm "/v4v-firewall-rules" tree) rules
    rules = [ ("/v4v-firewall-rules/0", "myself -> 0:4346709")
            , ("/v4v-firewall-rules/1", "myself -> 0:80")
            , ("/v4v-firewall-rules/2", "myself-if-seamless:14494 -> 0:4494")
            , ("/v4v-firewall-rules/3", "seamless -> myself-if-seamless:100")
            , ("/v4v-firewall-rules/4", "seamless:11494 -> myself-if-seamless:1494")
            , ("/v4v-firewall-rules/5", "myself -> 0:5556")
            ]

extSSH = do
  have <- doesFileExist "/config/ssh_enabled"
  when have $ do
           safeSpawnShell "mkdir -p /config/ssh"
           removeFile "/config/ssh_enabled"
           writeFile "/config/ssh/enabled" []

