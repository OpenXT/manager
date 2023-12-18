--
-- Copyright (c) 2015 Assured Information Security, Inc. <pattersonc@ainfosec.com>
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

{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- description: ac97 is now supported in windows guests with latest openxt tools
-- date: 04/30/2015

module Migrations.M_24 (migration) where

import UpgradeEngine
import Data.List (foldl')
import ShellTools
import Control.Monad
import System.Directory

migration = Migration {
              sourceVersion = 24
            , targetVersion = 25
            , actions = act
            }

act :: IO ()
act = updateAudioBackend

updateAudioBackend = xformVmJSON xform where
  xform tree = case jsGet "/os" tree of
    Just (jsUnboxString -> s)
      | (    s == "windows"
          || s == "windows8" ) -> jsSet "/config/sound" (jsBoxString "ac97") tree
    _ -> tree

