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

module Tools.UuidTable
    ( UuidTable
    -- * Creation
    , empty
    , singleton
    -- * Conversion
    , fromList
    , toList
    -- * Modification
    , insert
    , insertWith
    , insertWithKey
    , insertWith'
    , insertWithKey'
    , delete
    , update
    , updateWithKey
    -- * Reading
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , keys
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import qualified Data.Map as M
import Tools.Uuid (Uuid)
import Prelude hiding (null, lookup)

data UuidTable kind a = UuidTable { getRef :: MVar (M.Map (Uuid kind) a) }

withRead table f  = f <$> readMVar (getRef table)
withModify table f = modifyMVar_ (getRef table) (return . f)

empty :: IO (UuidTable kind a)
empty = UuidTable <$> newMVar M.empty

singleton k a = UuidTable <$> newMVar (M.singleton k a)

fromList l = UuidTable <$> (newMVar $ M.fromList l)

toList table = M.toList <$> (readMVar $ getRef table)

insert k a table = withModify table (M.insert k a)
insertWith f k a table = withModify table (M.insertWith f k a)
insertWithKey f k a table = withModify table (M.insertWithKey f k a)
insertWith' f k a table = withModify table (M.insertWith' f k a)
insertWithKey' f k a table = withModify table (M.insertWithKey' f k a)
delete k table = withModify table (M.delete k)
update f k table = withModify table (M.update f k)
updateWithKey f k table = withModify table (M.updateWithKey f k)

null table = withRead table M.null
size table = withRead table M.size
member k table = withRead table (M.member k)
notMember k table = withRead table (M.notMember k)
lookup k table = withRead table (M.lookup k)
findWithDefault a k table = withRead table (M.findWithDefault a k)
keys table = withRead table M.keys
