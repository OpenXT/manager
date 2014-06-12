--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

module Migrations.M_8 ( migration ) where

import UpgradeEngine

migration = Migration
    { sourceVersion = 8
    , targetVersion = 9
    , actions = act
    }

act :: IO ()
act = do
    xformVmJSON $ upgradeManifests

-- Derives the appliance type from the installed manifest
upgradeManifests :: JSValue -> JSValue
upgradeManifests tree =
    jsModify manifest "/backend/appliance_manifest" $
    tree
    where
      app_type True  _             = "dynamic"
      app_type False n | n > 1     = "hybrid"
                       | otherwise = "static"

      shared (Just (JSBool x)) = x
      shared _                 = error "expected appliance shared boolean"

      num_disks (Just (JSArray dms)) = length dms
      num_disks _                    = error "expected array of disk manifests"

      manifest tree =
          jsSet "/appliance/imgType" (jsBoxString $ app_type s n) $
          tree
          where
            s = shared $ jsGet "/appliance/shared" tree
            n = num_disks $ jsGet "/disk_manifests" tree
