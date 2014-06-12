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

module Appliance
       (
         App (..)
       , AppID (..)
       , AppVersion
       , AppAsset (..)
         
       , contentVirtualSystems
       )
       where

import System.FilePath
import Core.Types
import VirtualSystem

data App
   = App
     {
       appID :: AppID
     , appContent :: Content
     , appAssets :: [AppAsset]
     } deriving (Eq, Show)

data AppID = AppID String AppVersion deriving (Eq)
type AppVersion = Int

instance Show AppID where
  show (AppID id v) = id ++ "-" ++ show v

data AppAsset
   = ConfigFile FileResource FilePath
   | StorageFile FileResource FilePath
     deriving (Eq, Show)

contentVirtualSystems :: Content -> [VirtualSystem]
contentVirtualSystems (ContentVirtualSystem sys) = [sys]
contentVirtualSystems (ContentVirtualSystemCollection c) = concatMap contentVirtualSystems (collectionItems c)
