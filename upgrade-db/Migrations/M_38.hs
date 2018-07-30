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

module Migrations.M_38 (migration) where

import UpgradeEngine

migration = Migration {
              sourceVersion = 38
            , targetVersion = 39
            , actions = act
            }

act :: IO ()
act = updateSyncvm

updateSyncvm = xformVmJSON xform where
  xform tree = case jsGet "/type" tree of
    Just s | jsUnboxString s == "syncvm" -> modify tree
    _ -> tree
    where
      modify = jsSet "/config/flask-label"
                   (jsBoxString "system_u:system_r:syncvm_t") .
               jsRm "/config/extra-xenvm/0"
