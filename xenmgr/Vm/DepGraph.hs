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

module Vm.DepGraph
    ( DepGraph
    , depgraphFromEdges
    , dependencies
    ) where

import Data.List
import Data.Maybe

data DepGraph n = DepGraph [DepEdge n]
data DepEdge n = DepEdge n n

depgraphFromEdges :: [ (n,n) ] -> DepGraph n
depgraphFromEdges es = DepGraph $ map (\(a,b) -> DepEdge a b) es

edgeA (DepEdge a _) = a
edgeB (DepEdge _ b) = b

dependencies :: (Eq n) => n -> DepGraph n -> Maybe [n]
dependencies n g@(DepGraph es) = search [] n g
    where
      search visited n g@(DepGraph es)
          | n `elem` visited = Nothing -- cycles!
          | otherwise =
              let nei_deps = map (\n' -> search (n:visited) n' g) neighbours in
              case () of
                _ | all isJust nei_deps -> Just . nub $ (concatMap fromJust $ nei_deps) ++ neighbours
                  | otherwise           -> Nothing
          where
            neighbours =
                let is_neighbouring_edge n e = (edgeA e == n) in
                map edgeB $ filter (is_neighbouring_edge n) es
