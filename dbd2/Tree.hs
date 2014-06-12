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

module Tree
    ( Perm(..)
    , Tree(..)
    , toJSON
    , ofJSON
    , getSubtree
    , exists
    , valueGet
    , inject
    , valueSet
    , rm
    ) where

import Text.JSON
import Control.Arrow (second)
import Path

data Perm = Denied | Read | Write | ReadWrite
          deriving (Show,Eq)

data Tree = Empty
          | Leaf JSValue
          | FilePtr String
          | Nodes [(String,Tree)]
          deriving (Show,Eq)

toJSON :: Tree -> JSValue
toJSON Empty       = JSNull
toJSON (Leaf s)    = s
toJSON (FilePtr _) = JSNull
toJSON (Nodes ns)  = JSObject (toJSObject $ map (second toJSON) ns)

ofJSON :: JSValue -> Tree
ofJSON JSNull             = Empty
ofJSON j@(JSBool _)       = Leaf j
ofJSON j@(JSRational _ _) = Leaf j
ofJSON j@(JSString _)     = Leaf j
ofJSON j@(JSArray _)      = Leaf j
ofJSON (JSObject obj)     = Nodes $ map (second ofJSON) $ fromJSObject obj

getSubtree :: Path -> Tree -> Tree
getSubtree []     tree       = tree
getSubtree (p:ps) (Nodes ns) = maybe Empty (getSubtree ps) $ lookup p ns
getSubtree _      _          = Empty

exists :: Path -> Tree -> Bool
exists []     _          = True
exists (p:ps) (Nodes ns) = maybe False (exists ps) $ lookup p ns
exists _      _          = False

valueGet :: Path -> Tree -> Maybe JSValue
valueGet [] (Leaf s)       = Just s
valueGet [] (FilePtr s)    = Just $ JSString $ toJSString ("file:" ++ s)
valueGet [] _              = Nothing
valueGet (p:ps) (Nodes ns) = maybe Nothing (valueGet ps) $ lookup p ns
valueGet _      _          = Nothing

inject :: Path -> Tree -> Tree -> Tree
inject _      _ (FilePtr _) = error "cannot overwrite binary file nodes"
inject []     v _           = v
inject (p:ps) v Empty       = Nodes [(p, inject ps v Empty)]
inject (p:ps) v (Leaf _)    = Nodes [(p, inject ps v Empty)]
inject (p:ps) v (Nodes ns)  = maybe onNotFound onFound $ lookup p ns
    where onNotFound = Nodes (ns ++ [(p, inject ps v Empty)])
          onFound _  = Nodes $ lookupMap (inject ps v) p ns

valueSet :: Path -> JSValue -> Tree -> Tree
valueSet _      _ (FilePtr _) = error "cannot overwrite binary file nodes"
valueSet []     _ (Nodes ns)  = Nodes ns
valueSet []     v _           = Leaf v
valueSet (p:ps) v Empty       = Nodes [(p, valueSet ps v Empty)]
valueSet (p:ps) v (Leaf _)    = Nodes [(p, valueSet ps v Empty)]
valueSet (p:ps) v (Nodes ns)  = maybe onNotFound onFound $ lookup p ns
    where onNotFound = Nodes (ns ++ [(p,valueSet ps v Empty)])
          onFound _  = Nodes $ lookupMap (valueSet ps v) p ns

rm :: Path -> Tree -> Tree
rm _      (FilePtr _) = error "cannot overvalueSet binary file nodes"
rm []     _           = Empty
rm (_:_)  Empty       = Empty
rm (_:_)  t@(Leaf _)  = t
rm (p:ps) (Nodes ns)  = Nodes $ filter ((/=) Empty . snd) $ lookupMap (rm ps) p ns

lookupMap :: Eq a => (b -> b) -> a -> [(a,b)] -> [(a,b)]
lookupMap f x l = map (\(n,e) -> (n,if n == x then f e else e)) l
