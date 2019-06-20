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

module Domain where

import Data.Maybe
import Data.String
import Control.Applicative
import Types
import Tools.XenStore

uuidOfDomid :: DomID -> IO (Maybe Uuid)
uuidOfDomid domid =
    do path <- xsRead $ "/local/domain/" ++ show domid ++ "/vm"
       if isJust path
          then do uuid <- xsRead (fromJust path ++ "/uuid")
                  return . fmap fromString $ uuid
          else return Nothing

domidOfUuid :: Uuid -> IO (Maybe DomID)
domidOfUuid uuid =
    do domidStr <- xsRead $ "/xenmgr/vms/"++show uuid++"/domid"
       return . fmap read $ domidStr

stubdomTargetDomID :: DomID -> IO (Maybe DomID)
stubdomTargetDomID domid =
    fmap read <$> xsRead ("/local/domain/" ++ show domid ++ "/target")

invalidDomID :: Int
invalidDomID = 0x7FF4

currentDomain :: DomID
currentDomain = 0

