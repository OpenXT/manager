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

module Path where

import Utils (splitOn, Uuid)
import Data.List (intercalate)

type Path = [String]
type PathCred = (Path, Maybe Uuid)

pathOf :: String -> Path
pathOf = filter (/= "") . splitOn (== '/')

showPath :: Path -> String
showPath p = '/' : intercalate "/" p

isNullPath :: Path -> Bool
isNullPath = null

-- | check is @prefix is a prefix of @path
isPathPrefix :: Path -> Path -> Bool
isPathPrefix prefix path = loop prefix path
    where loop []     _      = True
          loop _      []     = False
          loop (x:xs) (y:ys)
            | x == y    = loop xs ys
            | otherwise = False

substractPrefix :: Path -> Path -> Path
substractPrefix prefix path = loop prefix path
    where loop []      p      = p
          loop _       []     = error "not a prefix"
          loop (x:xs)  (y:ys)
            | x == y          = loop xs ys
            | otherwise       = error "not a prefix"

matchPrefixNb :: Path -> Path -> Int
matchPrefixNb p1 p2 = loop 0 p1 p2
    where loop n []     _      = n
          loop n (x:xs) (y:ys)
            | x == y    = loop (n+1) xs ys
            | otherwise = n

parentPath :: Path -> Path
parentPath [] = []
parentPath l  = init l
