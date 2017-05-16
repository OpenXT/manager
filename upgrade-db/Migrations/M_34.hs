--
-- Copyright (c) 2017 Assured Information Security, Inc. <lejosnej@ainfosec.com>
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

-- description: Adjust VM settings for XL
-- date: 02/03/2017

module Migrations.M_34 (migration) where

import UpgradeEngine
import Data.List

migration = Migration {
              sourceVersion = 34
            , targetVersion = 35
            , actions = act
            }

act :: IO ()
act = do
  updateGuests

updateGuests = xformVmJSON xform where
  xform tree = case jsGet "/type" tree of
    Just s | jsUnboxString s == "svm" -> modify tree
    _ -> tree
    where
      modify = addRule 0 .
               jsMv "/config/acpi-pt"   "/config/acpi-table" .
               jsRm "/config/smbios-pt" .
               jsRm "/time-offset"
      addRule pos tree = case jsGet ("/v4v-firewall-rules/" ++ (show pos)) tree of
               Nothing -> jsSet ("/v4v-firewall-rules/" ++ (show pos)) (jsBoxString "my-stubdom -> 0:5100") tree
               Just _  -> addRule (pos + 1) tree
