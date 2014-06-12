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

module OVF.AllocationUnit
       (
         AllocationUnit (..)
       , AllocationUnitType (..)
       , allocationUnitParse
       , allocationUnitBytes
       , auByte
       ) where

import Data.Char
import Control.Applicative ( (<$>), (<*>), (*>), (<*), pure )
import Control.Monad
import Text.ParserCombinators.Parsec

data AllocationUnitType = AllocByte deriving Eq
data AllocationUnit = AllocationUnit { unitType :: AllocationUnitType, unitBase :: Integer, unitExp :: Integer } deriving Eq

instance Show AllocationUnitType where
  show AllocByte = "byte"
  
instance Show AllocationUnit where
  show (AllocationUnit t b e)
    = show t
       ++ if b == 1
             then ""
             else "*" ++ show b ++ if e == 0
                                      then ""
                                      else "^" ++ show e

auByte = AllocationUnit AllocByte 1 0

allocationUnitBytes :: AllocationUnit -> Integer
allocationUnitBytes u = case unitType u of
  AllocByte -> amount
  where
    amount = unitBase u ^ unitExp u

unit_p :: CharParser s AllocationUnit
unit_p = 
      try (AllocationUnit <$> type_p <*> (char '*' *> base_p) <*> (char '^' *> exp_p))
  <|> try (AllocationUnit <$> type_p <*> (char '*' *> base_p) <*> pure 0)
  <|> try (AllocationUnit <$> type_p <*> pure 1 <*> pure 0)
base_p = int_p
exp_p = int_p
int_p = many1 digit >>= return . (read :: String -> Integer)
type_p = string "byte" *> pure AllocByte

allocationUnitParse :: String -> Maybe AllocationUnit
allocationUnitParse str = either (const Nothing) sanitise $ parse (unit_p <* eof) "" str'
  where str' = filter (not . isSpace) str
        sanitise au | allocationUnitBytes au > 0 = Just au
                    | otherwise = Nothing
        
