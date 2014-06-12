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
module Migrations.M_12 (migration) where

import UpgradeEngine
import Data.List (foldl')

migration = Migration {
              sourceVersion = 12
            , targetVersion = 13
            , actions = act
            }

act :: IO ()
act = flask


flask = xformVmJSON xform where
    xform tree
        | Just t <- typ, t `elem` ["pvm","svm"] = set_flask tree
        | otherwise                             = tree
        where
          typ = jsUnboxString `fmap` (jsGet "/type" tree)

    set_flask tree =   jsSet "/config/flask-label" (jsBoxString "system_u:system_r:hvm_dom0IO_t")
                     . jsRm  "/config/extra-xenvm"
                     $ tree
