--
-- Copyright (c) 2012 Citrix Systems, Inc.
-- Copyright (c) 2018 Jason Andryuk
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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--

{-# LANGUAGE PatternGuards #-}

-- description: Change all v4v entries to argo entries
-- date: 06/12/2019

module Migrations.M_40 (migration) where

import UpgradeEngine

migration = Migration {
              sourceVersion = 40
            , targetVersion = 41
            , actions = act
            }

act :: IO ()
act = do
  updateV4VFirewallToArgo
  updateConfigV4VToArgo

updateV4VFirewallToArgo = xformVmJSON xform where
  xform tree = jsMv "/v4v-firewall-rules" "/argo-firewall-rules" tree

updateConfigV4VToArgo = xformVmJSON xform where
  xform tree = jsMv "/config/v4v" "/config/argo" tree
