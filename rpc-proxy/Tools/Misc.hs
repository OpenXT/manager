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

module Tools.Misc (
               split
             , strip
             , chomp
             ) where

import qualified Data.Text as T
import Control.Applicative
import System.Process
import System.IO
import System.Exit

-- chop line ending characters off
chomp :: String -> String
chomp s = case reverse s of
            ('\n' : '\r' : xs) -> reverse xs
            ('\n' : xs)        -> reverse xs
            _                  -> s

-- Split a list over an element
split :: (Eq a) => a -> [a] -> [[a]]
split sep xs =
    let (y,ys) = span (/= sep) xs in
    case ys of
      [] -> [y]
      zs -> y : split sep (tail zs)


-- Strip a string from whitespace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

