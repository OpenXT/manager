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
-- created 4.10.2012 (just after 3.0.0)
module Migrations.M_16 (migration) where

import UpgradeEngine

migration = Migration {
              sourceVersion = 16
            , targetVersion = 17
            , actions = act
            }

act :: IO ()
act = updateNilfvm

updateNilfvm = xformVmJSON xform where
  xform tree = case jsGet "/type" tree of
    Just s | jsUnboxString s == "nilfvm" -> modify tree
    _ -> tree
    where
      modify tree = f tree $ jsGet "/config/kernel-extract" tree where
        f tree Nothing = jsSet "/config/kernel-extract" (jsBoxString "/boot/vmlinuz") tree
        f tree _ = tree
        
