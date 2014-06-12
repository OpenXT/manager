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

module Types ( DomID
             , Uuid
             , uuidText
             ) where

import Data.String
import Data.Int
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

type DomID = Int32
newtype Uuid = Uuid { uuidText :: Text } deriving ( Eq, Ord )

instance Show Uuid where
    show (Uuid t) = TL.unpack t

instance IsString Uuid where
    fromString = Uuid . TL.pack
