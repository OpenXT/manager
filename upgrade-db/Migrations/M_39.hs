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

-- description: set ndvm to proper kernel path
-- date: 06/14/2016

module Migrations.M_39 (migration) where

import UpgradeEngine
import Data.List (foldl')

migration = Migration {
              sourceVersion = 39
            , targetVersion = 40
            , actions = act
            }

act :: IO ()
act = updateVirtType

updateVirtType = xformVmJSON xform where
  xform tree = case jsGet "/config/hvm" tree of
    Just s | jsUnboxString s == "true"  -> makeHvm tree
           | jsUnboxString s == "false" -> makePv  tree
    _ -> tree
    where
      makeHvm = jsSet "/config/virt-type" (jsBoxString "hvm") .
                jsRm  "/config/hvm"
      makePv  = jsSet "/config/virt-type" (jsBoxString "pv") .
                jsRm  "/config/hvm"
