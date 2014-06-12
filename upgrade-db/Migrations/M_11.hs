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
module Migrations.M_11 (migration) where

import UpgradeEngine
import Data.List (foldl')

--
-- GLENN ( post-brian ) - first migration
--

migration = Migration {
              sourceVersion = 11
            , targetVersion = 12
            , actions = act
            }

act :: IO ()
act = do
  v4vRules

v4vRules = xformVmJSON xform where
    xform tree
        | Just t <- typ, t `elem` ["pvm","svm"] = add_rules tree
        | otherwise                             = tree
        where
          typ = jsUnboxString `fmap` (jsGet "/type" tree)

    add_rules tree = foldl' (\t (p,v) -> jsSet p (jsBoxString v) t) tree rules
    rules = [ ("/v4v-firewall-rules/0", "myself -> 0:4346709")
            , ("/v4v-firewall-rules/1", "myself -> 0:80")
            , ("/v4v-firewall-rules/2", "myself-if-seamless:14494 -> 0:4494")
            , ("/v4v-firewall-rules/3", "seamless -> myself-if-seamless:100")
            , ("/v4v-firewall-rules/4", "seamless:11494 -> myself-if-seamless:1494")
            , ("/v4v-firewall-rules/5", "myself -> 0:5556")
            , ("/v4v-firewall-rules/6", "my-stubdom -> 0:5555")
            , ("/v4v-firewall-rules/7", "my-stubdom -> 0:4001")
            , ("/v4v-firewall-rules/8", "my-stubdom -> 0:4002")
            , ("/v4v-firewall-rules/9", "my-stubdom -> 0:5000")
            , ("/v4v-firewall-rules/10", "my-stubdom -> 0:5001")
            , ("/v4v-firewall-rules/11", "my-stubdom -> 0:5559")
            ]


