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

--
-- POST-ICHIBAN
--
module Migrations.M_10 ( migration ) where

import UpgradeEngine

migration = Migration
    { sourceVersion = 10
    , targetVersion = 11
    , actions = act
    }

act :: IO ()
act = do
    xformVmJSON $ removeEmptyDiskSnapshotFields

-- Remove all leaves of the form "/config/disk/*/snapshot": ""
removeEmptyDiskSnapshotFields :: JSValue -> JSValue
removeEmptyDiskSnapshotFields tree =
    foldl removeIfEmpty tree (jsGetChildren "/config/disk" tree)
    where
        removeIfEmpty tree diskId =
            let diskSnapshotPath = diskId ++ "/snapshot" in
            case jsGet diskSnapshotPath tree of
                Nothing                        ->                       tree
                Just x | jsUnboxString x == "" -> jsRm diskSnapshotPath tree
                       | otherwise             ->                       tree

