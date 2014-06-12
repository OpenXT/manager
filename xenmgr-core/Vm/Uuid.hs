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

module Vm.Uuid (
                 Uuid
               , uuidGen
               , uuidStr
               , uuidStrUnderscore
               , emptyUuid
           ) where

import Data.Maybe
import Data.String
import qualified Data.Text as T
import Rpc.Core

newtype Uuid = Uuid String deriving (Eq,Ord)

instance Show Uuid where
    show (Uuid s) = s

instance IsString Uuid where
    fromString str = Uuid str

--
-- Make Uuid instance of Variable for easier marshalling
--
instance Variable Uuid where
    toVariant uuid =
        toVariant (show uuid)

    fromVariant v =
      fmap Uuid $ fromVariant v

uuidGen :: IO Uuid
uuidGen =
    readFile "/proc/sys/kernel/random/uuid" >>= return . fromString . strip
  where strip = T.unpack . T.strip . T.pack

uuidStr :: Uuid -> String
uuidStr = show

uuidStrUnderscore :: Uuid -> String
uuidStrUnderscore uuid =
    map subst $ show uuid
  where
    subst '-' = '_'
    subst ch  = ch

emptyUuid :: Uuid
emptyUuid = fromString ""
