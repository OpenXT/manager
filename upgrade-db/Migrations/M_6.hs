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

{-# LANGUAGE PatternGuards #-}
module Migrations.M_6 (migration) where

import UpgradeEngine
import Data.List

migration = Migration {
              sourceVersion = 6
            , targetVersion = 7
            , actions = act
            }

act :: IO ()
act = do
  xformVmJSON $ scrap_view_seamless_apps . scrap_pvm
  v4vRules
  backendUpgrade
  where
    scrap_view_seamless_apps = jsRm "/view-seamless-applications"
    scrap_pvm tree | jsGet "/type" tree == Just (jsBoxString "pvm") =
                       jsSet "/type" (jsBoxString "svm")
                     . jsSet "/gpu"  (jsBoxString "hdx")
                     $ tree
                   | otherwise =
                       tree

v4vRules = xformVmJSON xform where
    xform tree
        | Just t <- typ, t `elem` ["svm"] = add_rules tree
        | otherwise                       = tree
        where
          typ = jsUnboxString `fmap` (jsGet "/type" tree)

    add_rules tree = foldl' (\t (p,v) -> jsSet p (jsBoxString v) t) (jsRm "/v4v-firewall-rules" tree) rules
    rules = [ ("/v4v-firewall-rules/0", "myself -> 0:4346709")
            , ("/v4v-firewall-rules/1", "myself -> 0:80")
            , ("/v4v-firewall-rules/2", "myself-if-seamless:14494 -> 0:4494")
            , ("/v4v-firewall-rules/3", "seamless -> myself-if-seamless:100")
            , ("/v4v-firewall-rules/4", "seamless:11494 -> myself-if-seamless:1494")
            ]
backendUpgrade :: IO ()
backendUpgrade = do
  xformVmJSON $ upgradeManifest
  where
    upgradeManifest tree =
        jsModify policy "/backend/appliance_manifest/appliance/policy" .
        jsRm "/backend/appliance_manifest/appliance/config/type" $
        tree
        where
          threed_enabled = jsGet "/gpu" tree == Just (jsBoxString "hdx")
          policy tree =
              jsSet "/threeDGraphics/isEnabled" (JSBool threed_enabled) .
              jsSet "/autostart/isEnabled" (JSBool False) .
              jsSet "/seamlessAppSharing/isEnabled" (JSBool True) $
              tree

