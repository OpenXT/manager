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
module Migrations.M_15 (migration) where

import UpgradeEngine
import Data.List (foldl')

migration = Migration {
              sourceVersion = 15
            , targetVersion = 16
            , actions = act
            }

act :: IO ()
act = fixRpcAgent

-- rpc-agent flag was removed. Assume all vms which had it set
-- are linuxes
fixRpcAgent = xformVmJSON xform where
  xform tree 
    | jsGet "/rpc-agent" tree == Just (jsBoxString "true") = jsRm "/rpc-agent" . jsSet "/os" (jsBoxString "linux") $ tree
    | otherwise = tree
