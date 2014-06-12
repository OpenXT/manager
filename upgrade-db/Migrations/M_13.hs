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

{-# LANGUAGE PatternGuards #-}
module Migrations.M_13 (migration) where

import UpgradeEngine
import Data.List (foldl')

migration = Migration {
              sourceVersion = 13
            , targetVersion = 14
            , actions = act
            }

act :: IO ()
act = do
  networking
  backendUpgrade

networking = xformVmJSON xform where
  xform = jsMapChildren xformNic "/config/nic"
  xformNic = addWirelessDriver . fixValue . renameKey
  renameKey = jsMv "bridge" "network"
  fixValue tree
    = maybe tree (\v -> jsSet "network" (fix v) tree)  (jsGet "network" tree)
      where fix = jsBoxString . fix' . jsUnboxString
            fix' "brbridged" = "/wired/0/bridged"
            fix' "brshared" = "/wired/0/shared"
            fix' "brwireless" = "/wifi/0/shared"
            fix' "brinternal" = "/internal/0"
            fix' "/internal" = "/internal/0"
            fix' "/icavm" = "/any/0"
            fix' "/wireless/0/shared" = "/wifi/0/shared"
            fix' other = other
  addWirelessDriver tree
    | jsGet "network" tree == Just (jsBoxString "/wifi/0/shared")
      = jsSet "wireless-driver" (jsBoxString "true") tree
    | otherwise = tree

backendUpgrade = xformVmJSON xform where
  xform = jsMapChildren xformDiskType "/config/disk"
  xformDiskType = jsMv "disktype" "managed-disktype"
