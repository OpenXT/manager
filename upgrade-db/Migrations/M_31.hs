--
-- Copyright (c) 2016 Assured Information Security, Inc. <lejosnej@ainfosec.com>
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

-- description: Fix the guest v4v firewall rules
-- date: 08/25/2016

module Migrations.M_31 (migration) where

import UpgradeEngine
import Data.List

migration = Migration {
              sourceVersion = 31
            , targetVersion = 32
            , actions = act
            }

act :: IO ()
act = do
  v4vRules

v4vRules = xformVmJSON xform where
    xform tree
        | Just t <- typ, t `elem` ["svm"] = add_rules tree
        | otherwise                       = tree
        where
          typ = jsUnboxString `fmap` (jsGet "/type" tree)

    add_rules tree = foldl' (\t (p,v) -> jsSet p (jsBoxString v) t) (jsRm "/v4v-firewall-rules" tree) rules
    rules = [ ("/v4v-firewall-rules/0", "my-stubdom -> 0:4001")
            , ("/v4v-firewall-rules/1", "my-stubdom -> 0:4002")
            , ("/v4v-firewall-rules/2", "my-stubdom -> 0:5000")
            , ("/v4v-firewall-rules/3", "my-stubdom -> 0:5001")
            ]
