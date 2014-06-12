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

{-# LANGUAGE PatternGuards #-}
-- Helpers for manipulating json trees
module JSONTrees ( module Text.JSON
                 , jsGet
                 , jsGetChildren
                 , jsSet
                 , jsModify
                 , jsMapChildren
                 , jsRm
                 , jsMv
                 , jsCp
                 , jsBoxString
                 , jsUnboxString
                 , jsPretty ) where

import Data.List
import Data.Maybe
import Text.JSON
import qualified JSONPretty as Pretty

type JSONTreePath = [String]
type Tree = JSValue

pathOfString :: String -> JSONTreePath
pathOfString path = filter (not.null) $ split '/' path

stringOfPath :: JSONTreePath -> String
stringOfPath path = "/" ++ (concat . intersperse "/" $ path)

jsPretty :: JSValue -> String
jsPretty v = (show . Pretty.pp_value $ v)

jsGet :: String -> Tree -> Maybe JSValue
jsGet path tree = aux tree (pathOfString path) where
    aux tree            []     = Just tree
    aux (JSObject tree) (p:ps) = namedChild tree p >>= \child -> aux child ps
    aux _               _      = Nothing

jsGetChildren :: String -> Tree -> [String]
jsGetChildren path tree = aux path (jsGet path tree) where
    aux path (Just (JSObject obj)) = map stringOfPath . map (\n -> path' ++ [n]) $ childNames obj
        where
          path' = pathOfString path
          childNames obj = map fst . fromJSObject $ obj
    aux path tree = []

jsSet :: String -> JSValue -> Tree -> Tree
jsSet path v tree = aux tree (pathOfString path) v where
    aux tree           []     v = v
    aux (JSObject obj) (p:ps) v =
        JSObject . replaceOrAddChild obj p $ aux subtree ps v
            where subtree | Just sv <- namedChild obj p = sv
                          | otherwise                   = JSObject . toJSObject $ []
    aux _ _ _ = error "unexpected json entity"

jsModify :: (Tree -> Tree) -> String -> Tree -> Tree
jsModify f path tree =
    case jsGet path tree of
      Nothing -> tree
      Just v  -> jsSet path (f v) tree

jsRm :: String -> Tree -> Tree
jsRm path tree = aux tree (pathOfString path) where
    aux tree [] = JSObject . toJSObject $ []
    aux (JSObject obj) [p] = JSObject $ rmChild obj p
    aux (JSObject obj) (p:ps) | Just child <- namedChild obj p = JSObject . replaceOrAddChild obj p $ aux child ps
                              | otherwise                      = JSObject $ obj
    aux _ _ = error "unexpected json entity"

jsMv :: String -> String -> Tree -> Tree
jsMv from to tree =
    case jsGet from tree of
      Nothing  -> tree
      Just src -> jsSet to src (jsRm from tree)

jsCp :: String -> String -> Tree -> Tree
jsCp from to tree =
    case jsGet from tree of
      Nothing  -> tree
      Just src -> jsSet to src tree


jsMapChildren :: (Tree -> Tree) -> String -> Tree -> Tree
jsMapChildren f path tree = foldl xform tree children where
    xform t path = case jsGet path t of
                     Nothing -> error $ "expected value at " ++ path
                     Just v  -> jsSet path (f v) t
    children     = jsGetChildren path tree

jsBoxString :: String -> JSValue
jsBoxString = JSString . toJSString

jsUnboxString :: JSValue -> String
jsUnboxString (JSString s) = fromJSString s
jsUnboxString _            = error "string json value was expected"

replaceOrAddChild :: JSObject JSValue -> String -> JSValue -> JSObject JSValue
replaceOrAddChild obj name v =
    case namedChild obj name of
      Nothing -> addChild obj name v
      _       -> replaceChild obj name v

replaceChild :: JSObject JSValue -> String -> JSValue -> JSObject JSValue
replaceChild obj name v = toJSObject . map replace . fromJSObject $ obj where
    replace (c_name, c_v) | c_name == name = (c_name,   v)
                          | otherwise      = (c_name, c_v)

addChild :: JSObject JSValue -> String -> JSValue -> JSObject JSValue
addChild obj name v = toJSObject . add . fromJSObject $ obj where
    add xs = xs ++ [ (name, v) ]

rmChild :: JSObject JSValue -> String -> JSObject JSValue
rmChild obj name = toJSObject . filter (not . named name) . fromJSObject $ obj where
    named expected (actual,_) = expected == actual

namedChild :: JSObject JSValue -> String -> Maybe JSValue
namedChild obj name = fmap snd $ find (named name) (fromJSObject obj) where
    named expected (actual,_) = expected == actual

-- Split a list over an element
split :: (Eq a) => a -> [a] -> [[a]]
split sep xs =
    let (y,ys) = span (/= sep) xs in
    case ys of
      [] -> [y]
      zs -> y : split sep (tail zs)
