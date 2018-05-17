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

module Tools.PartTable where

import Data.Char
import Data.List
import Data.Maybe
import Data.Int
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import System.IO
import Tools.Process
import Tools.Text

data Partition
   = Partition { partNum :: Int
               , partStart :: Int64 }
     deriving ( Eq, Show )
type PartTable = [Partition]

readPartTable :: FilePath -> IO (Maybe PartTable)
readPartTable dev
  = parse <$> fdisk where
    fdisk = readProcessOrDie "fdisk" ["-l", "-u", dev] ""
    parse out = table =<< (tableStart . lines $ out)
    tableStart ls = case dropWhile (not . tableHdr) ls of
      [] -> Nothing
      (_:xs) -> Just xs
    tableHdr = (== ["Device","Boot","Start","End","Sectors","Size","Id","Type"]) . words
    table = Just . catMaybes . map partLine
    partLine l = Partition <$> num ws <*> start ws
                 where ws = words l
    num [] = Nothing
    num (x:_) = maybeRead . reverse . takeWhile isDigit . reverse $ x

    start (_:"*":xs) = start' xs
    start (_:xs) = start' xs
    start _ = Nothing
    start' [] = Nothing
    start' (x:_) = (*512) <$> maybeRead x
