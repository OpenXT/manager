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

module Migrations.M_2 (migration) where

import System.IO.Unsafe
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.List
import FileTools
import ShellTools
import UpgradeEngine

migration = Migration {
              sourceVersion = 2
            , targetVersion = 3
            , actions = act
            }

act :: IO ()
act = do
  backendUpgrade

backendUpgrade :: IO ()
backendUpgrade = do
    xformVmJSON $ vmUpgrade
    xformPrimaryJSON $ \tree -> jsSet bedVersion (jsBoxString "2") tree
  where
    bedVersion = "/com.citrix.xenclient.backend/bed-version"
    -- only xform if bed schema is version 1
    vmUpgrade tree | jsGet bedVersion tree == (Just . jsBoxString $ "1") = upgradeManifest tree
                   | otherwise                                           = tree

    upgradeManifest tree =
        jsSet "/backend/appliance_manifest/appliance/policy/backend/allowManualBackups" (JSBool False) .
        jsModify disks "/backend/appliance_manifest/disk_manifests" .
        jsModify (scheduledOps "manifest") "/backend/scheduled_checkouts" .
        jsModify (scheduledOps "manifest") "/backend/scheduled_checkins" .
        jsModify (scheduledOps "manifest") "/backend/scheduled_updates" .
        jsModify (scheduledOps "manifest") "/backend/scheduled_force_updates" .
        jsModify (scheduledOps "app_manifest") "/backend/scheduled_restores" .
        jsModify (scheduledOps "inst_manifest") "/backend/scheduled_restores" $
        tree
        where
          scheduledOps m (JSArray ops) = JSArray . map (jsModify disks (m++"/disk_manifests")) $ ops
          scheduledOps _ _             = error "expected array of scheduled operations"
          disks (JSArray dms)  = JSArray . map (jsModify images "image_manifests") $ dms
          disks _              = error "expected array of disk manifests"
          images (JSArray ims) = JSArray . map (jsModify image "disk_image") $ ims
          images _             = error "expected array of image manifests"
          image tree  = jsRm "URL" tree

