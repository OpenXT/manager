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

{-# LANGUAGE ScopedTypeVariables #-}
module StoreHouse.VHD.Graph
    ( VHDGraph
    , newVHDGraph
    -- * query stuff
    , getChildren
    , getParents
    , getLinks
    -- * add stuff
    , addEnt
    , addParent
    , addChild
    -- * remove stuff
    , delEnt

    -- * init
    , initWithPaths
    , appendPath
    ) where

import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import Tools.Uuid
import qualified Data.Map as M
import Control.Concurrent.MVar
import qualified Data.Vhd as Vhd
import System.FilePath

type VHDLink = ([Uuid KindVHD], [Uuid KindVHD])

data VHDGraph_ = VHDGraph_
    { table :: M.Map (Uuid KindVHD) VHDLink
    }

newtype VHDGraph = VHDGraph (MVar VHDGraph_)

newVHDGraph = VHDGraph <$> newMVar (VHDGraph_ M.empty)

withGraph :: VHDGraph -> (VHDGraph_ -> IO a) -> IO a
withGraph (VHDGraph graph) f = withMVar graph f

withGraphModify :: VHDGraph -> (VHDGraph_ -> IO (VHDGraph_, a)) -> IO a
withGraphModify (VHDGraph graph) f = modifyMVar graph f

getChildren_ uuid graph = maybe [] fst $ M.lookup uuid (table graph)
getParents_ uuid graph = maybe [] snd $ M.lookup uuid (table graph)

getChildren :: Uuid KindVHD -> VHDGraph -> IO [Uuid KindVHD]
getChildren uuid graph = withGraph graph (return . getChildren_ uuid)

getParents :: Uuid KindVHD -> VHDGraph -> IO [Uuid KindVHD]
getParents uuid graph = withGraph graph (return . getParents_ uuid)

getLinks :: Uuid KindVHD
         -> VHDGraph                            -- ^ VHD graph
         -> IO ([Uuid KindVHD], [Uuid KindVHD]) -- ^ get the list of children and parents
getLinks uuid graph = withGraph graph $ \graph_ -> do
    let parents  = getParents_ uuid graph_
        children = getChildren_ uuid graph_
    return (parents, children)

addParent_ uuid parent graph_ = graph_ { table = M.alter addP uuid (table graph_) }
    where addP Nothing = Just ([], [parent])
          addP (Just (children, parents)) = Just (children, parent: parents)

addParent :: Uuid KindVHD -- ^ vhd uuid
          -> Uuid KindVHD -- ^ parent to add
          -> VHDGraph     -- ^ the VHD graph
          -> IO ()
addParent uuid parent graph = addChild parent uuid graph

addChild :: Uuid KindVHD -- ^ vhd uuid
         -> Uuid KindVHD -- ^ child to add
         -> VHDGraph     -- ^ the VHD graph
         -> IO ()
addChild uuid child graph = withGraphModify graph $ \graph_ ->
    return (addParent_ child uuid $ graph_ { table = M.alter addC uuid (table graph_) }, ())
    where addC Nothing = Just ([child], [])
          addC (Just (children,parents)) = Just (child : children, parents)

addEnt uuid graph = withGraphModify graph $ \graph_ ->
    return (graph_ { table = M.insert uuid ([], []) (table graph_) }, ())

delEnt uuid graph = withGraphModify graph $ \graph_ ->
    case M.lookup uuid (table graph_) of
        Nothing                 -> return (graph_, ())
        Just (children,parents) -> do
            let ntable = M.delete uuid $ flip (foldl alterParent) parents $ flip (foldl alterChildren) children $ table graph_
            return (graph_ { table = ntable }, ())
    where alterParent m n = M.alter delC n m
          alterChildren m n = M.alter delP n m
          delC Nothing                   = Nothing -- something is not tight as it should. maybe raise an error ?
          delC (Just (children,parents)) = Just (filter ((/=) uuid) children, parents)
          delP Nothing                   = Nothing
          delP (Just (children,parents)) = Just (children, filter ((/=) uuid) parents)
          
initWithPaths dir paths = newVHDGraph >>= \graph -> mapM_ (appendPath graph dir) paths >> return graph

appendPath graph dir path = withVhd (dir </> path) $ \header footer -> do
      let uuid  = castUuid $ Vhd.footerUniqueId footer
      let puuid = castUuid $ Vhd.headerParentUniqueId header
      addEnt uuid graph >> addParent uuid puuid graph
    where castUuid :: Vhd.UniqueId -> Uuid KindVHD
          castUuid (Vhd.UniqueId b) = uuidFromBinary b

withVhd path f = do
    e <- try $ Vhd.getInfo path
    case e of
        Right (Right (header, footer)) -> f header footer
        Right (Left err)               -> return ()
        Left (exn :: SomeException)    -> return ()
