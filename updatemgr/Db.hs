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

{-# LANGUAGE TypeSynonymInstances,OverlappingInstances,TypeOperators,FlexibleInstances,UndecidableInstances #-}

-- Higher level interface for database access
-- which allows to read / write any types implementing Marshall class
module Db (
            Marshall, DbRepr (..)
          , DbTree (..), Path
          , dbRead
          , dbReadWithDefault
          , dbMaybeRead
          , dbWrite
          , dbRm
          , dbExists
          , dbListPaths
          , dbList
          , dbTreeToStringMap
          ) where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Int
import Data.String
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Rpc.Core
import Rpc.Autogen.DbClient
import Tools.Misc
import Tools.IfM

type Path = String

-- Tree of strings really
data DbTree = Leaf String | Record [ (String,DbTree) ] deriving ( Eq, Show )

instance (IsRemoteError e, MonadRpc e m) => Applicative m where
    pure  = return
    (<*>) = ap

instance (IsRemoteError e, MonadRpc e m) => Functor m where
    fmap = ap . return

-- Db tree can be serialised into database daemon
instance Marshall DbTree where
    dbWrite p (Leaf v) =
        dbWrite p v
    dbWrite p (Record tuples) =
        mapM_ write tuples
            where write (name,v) = dbWrite (p ++ "/" ++ name) v

    dbRead p = dbList p >>= \paths ->
               case paths of
                 [] -> Leaf <$> dbRead p
                 cs -> do
                   let ps = map (\c -> p ++ "/" ++ c) cs
                   vs <- mapM dbRead ps
                   return $ Record (zip cs vs)

-- Something which has a database tree representation
class DbRepr a where
    fromDbTree :: DbTree -> Maybe a
    toDbTree :: a -> DbTree

-- DbRepr type can be marshalled easily
instance (Eq a, DbRepr a) => Marshall a where
    dbWrite p v = dbWrite p (toDbTree v)
    dbRead p = dbRead p >>= \tree -> case fromDbTree tree of
                                       Nothing -> error ("failed to parse DB tree: " ++ show tree)
                                       Just v  -> return v

-- Convert a db tree type map of (key,value) string pairs
dbTreeToStringMap :: DbTree -> M.Map String String
dbTreeToStringMap (Leaf _) = M.empty
dbTreeToStringMap (Record tuples) =
    M.unions (map submap tuples)
  where
    submap (name, Leaf v) = M.singleton name v
    submap (name, other ) = prefix_key (dbTreeToStringMap other) where
                            prefix_key = M.mapKeys (\k -> name ++ "/" ++ k)

service = "com.citrix.xenclient.db"
objpath = "/"
call f  = f service objpath

-- Read from database. Empty string when DB node does not exist
dbReadStr :: (IsRemoteError e, MonadRpc e m) => Path -> m String
dbReadStr path = call comCitrixXenclientDbRead path

-- Write to database
dbWriteStr :: (IsRemoteError e, MonadRpc e m) => Path -> String -> m ()
dbWriteStr path value = call comCitrixXenclientDbWrite path value

-- List child nodes of a given node
dbList :: (IsRemoteError e, MonadRpc e m) => Path -> m [String]
dbList path = call comCitrixXenclientDbList path

-- Check if a path exists
dbExists :: (IsRemoteError e, MonadRpc e m) => Path -> m Bool
dbExists path =  call comCitrixXenclientDbExists path

-- List child paths of a given node
dbListPaths :: (IsRemoteError e, MonadRpc e m) => Path -> m [Path]
dbListPaths path = do
    nodes <- dbList path
    return $ map (join path) nodes
  where
    join x y = x ++ "/" ++ y

-- Remove a node with subnodes
dbRm :: (IsRemoteError e, MonadRpc e m) => Path -> m ()
dbRm path = call comCitrixXenclientDbRm path

dbInject :: (IsRemoteError e, MonadRpc e m) => Path -> String -> m ()
dbInject path value = call comCitrixXenclientDbInject path value

--
-- Class for types which can be marshalled from / to database
--
class (Eq p) => Marshall p where
    dbRead  :: (IsRemoteError e, MonadRpc e m) => Path -> m p
    dbWrite :: (IsRemoteError e, MonadRpc e m) => Path -> p -> m ()

-- String can be easily parshalled
instance Marshall String where
    dbRead      = dbReadStr
    dbWrite     = dbWriteStr
-- Integer can do it too
instance Marshall Int where
    dbRead      = fmap Prelude.read . dbReadStr
    dbWrite x   = dbWriteStr x . show

instance Marshall Int32 where
    dbRead      = fmap Prelude.read . dbReadStr
    dbWrite x   = dbWriteStr x . show

instance Marshall Double where
    dbRead      = fmap Prelude.read . dbReadStr
    dbWrite x   = dbWriteStr x . show

-- Boolean can do it too
instance Marshall Bool where
    dbRead  x   = fmap fromS . dbReadStr $ x
              where
                -- People write booleans in all kind of ways..
                fromS "true"  = True
                fromS "yes"   = True
                fromS "1"     = True
                fromS "false" = False
                fromS "no"    = False
                fromS "0"     = False
                fromS s       = error $ "unexpected boolean text representation: " ++ s ++ " while reading " ++ x

    dbWrite x   = dbWriteStr x . toS
              where
                -- But we write them only in one true way
                toS True  = "true"
                toS False = "false"

-- List of marshalled types is marshalled as well
instance (Marshall a) => Marshall [a] where
    dbRead  x      = dbListPaths x >>= mapM dbRead
    dbWrite x vs   = dbRm x        >>  mapM_ setOne (zip paths vs)
                     where paths        = map (\id -> x ++ "/" ++ (show id)) [0..]
                           setOne (p,v) = dbWrite p v

-- maps of marshalable types are marshalable
instance (Ord k, Show k, IsString k, Marshall k, Marshall v) => Marshall (M.Map k v) where
    dbRead x    = M.fromList <$> pairs
                  where
                    pairs    = zip <$> ids <*> objs
                    ids      = map fromString <$> dbList x
                    id_paths = map (\id -> x ++ "/" ++ show id) <$> ids
                    objs     = mapM dbRead =<< id_paths

    dbWrite x v = do mapM write (sortByID . M.toList $ v)
                     -- need to remove the disks which are not in the map from db
                     mapM_ remove =<< excessIds
                  where
                    write (id,obj)  = dbWrite (x ++ "/" ++ show id) obj
                    remove id       = dbRm (x ++ "/" ++ show id)
                    ids             = M.keys v
                    prevIds         = map fromString <$> dbList x
                    excessIds       = prevIds `minus` (return ids)
                    a `minus` b     = differenceList <$> a <*> b
                    sortByID        = sortBy (comparing fst)

-- Read database node if it exists, otherwise return nothing
dbMaybeRead :: (IsRemoteError e, MonadRpc e m, Marshall a) => Path -> m (Maybe a)
dbMaybeRead p = ifM (dbExists p)
                (Just <$> dbRead p)
                (return Nothing)

dbReadWithDefault :: (IsRemoteError e, MonadRpc e m, Marshall a) => a -> Path -> m a
dbReadWithDefault d = fmap (fromMaybe d) . dbMaybeRead
