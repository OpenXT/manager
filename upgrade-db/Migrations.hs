--
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

module Migrations ( getMigrationFromVer ) where

import Data.List
import Data.Maybe
import UpgradeEngine
import qualified Migrations.M_1
import qualified Migrations.M_2
import qualified Migrations.M_3
import qualified Migrations.M_4
import qualified Migrations.M_5
import qualified Migrations.M_6
import qualified Migrations.M_7
import qualified Migrations.M_8
import qualified Migrations.M_9
import qualified Migrations.M_10
import qualified Migrations.M_11
import qualified Migrations.M_12
import qualified Migrations.M_13
import qualified Migrations.M_14
import qualified Migrations.M_15
import qualified Migrations.M_16
import qualified Migrations.M_17
import qualified Migrations.M_18
import qualified Migrations.M_19
import qualified Migrations.M_20
import qualified Migrations.M_21
import qualified Migrations.M_22
import qualified Migrations.M_23
import qualified Migrations.M_24
import qualified Migrations.M_25
import qualified Migrations.M_26
import qualified Migrations.M_27
import qualified Migrations.M_28
import qualified Migrations.M_29

migrations :: [Migration]
migrations = [ Migrations.M_1.migration
             , Migrations.M_2.migration
             , Migrations.M_3.migration
             , Migrations.M_4.migration
             , Migrations.M_5.migration
             , Migrations.M_6.migration
             , Migrations.M_7.migration
             , Migrations.M_8.migration
             , Migrations.M_9.migration
             , Migrations.M_10.migration
             , Migrations.M_11.migration
             , Migrations.M_12.migration
             , Migrations.M_13.migration
             , Migrations.M_14.migration
             , Migrations.M_15.migration
             , Migrations.M_16.migration
             , Migrations.M_17.migration
             , Migrations.M_18.migration
             , Migrations.M_19.migration
             , Migrations.M_20.migration
             , Migrations.M_21.migration
             , Migrations.M_22.migration
             , Migrations.M_23.migration
             , Migrations.M_24.migration
             , Migrations.M_25.migration
             , Migrations.M_26.migration
             , Migrations.M_27.migration
             , Migrations.M_28.migration
             , Migrations.M_29.migration
             ]

getMigrationFromVer :: Int -> Migration
getMigrationFromVer version =
    let Just m = find pred migrations in m
  where
    pred m = sourceVersion m == version
