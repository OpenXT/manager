--
-- Copyright (c) 2016 Assured Information Security, Inc.
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

-- description: disable cdrom auto-assign by default
-- date: 06/27/2016

module Migrations.M_29 (migration) where

import UpgradeEngine

migration = Migration {
              sourceVersion = 29
            , targetVersion = 30
            , actions = act
            }

act :: IO ()
act = do
    xformPrimaryJSON $ jsSet "/xenmgr/autolock-cd-drives" (JSBool False)
