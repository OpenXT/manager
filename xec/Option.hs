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

module Option where

data OptSpec
   = OptSpec { long :: String
             , short :: String
             , argsSpec :: OptArgsSpec }

data OptArgsSpec
   = OneArgSpec | TwoArgSpec | NoArgSpec

data Opt  = Opt { label :: String, args :: Args } deriving Show
data Args = OneArg String | TwoArg String String | NoArg deriving Show

oneArg = OneArgSpec
twoArg = TwoArgSpec
noArg = NoArgSpec

parse :: [OptSpec] -> [String] -> Maybe ( [Opt], [String] )
parse specs xs = go xs [] where
  go [] opts = Just ( opts, [] )
  go (x:xs) opts
    = let matches = filter (match x) specs in
    case matches of
      []    -> Just ( opts, x:xs )
      (m:_) -> consume m
    where
      consume m =
        case argsSpec m of
          NoArgSpec  -> go xs (opts ++ [Opt (long m) NoArg])
          OneArgSpec -> case xs of
                (a:ys)   -> go ys (opts ++ [Opt (long m) (OneArg a)])
                _        -> go [] opts
          TwoArgSpec -> case xs of
                (a:b:ys) -> go ys (opts ++ [Opt (long m) (TwoArg a b)])
                _        -> go [] opts

  match x spec =
         ("--" ++ long spec == x)
      || (x `elem` (map (\c -> '-':c:[]) $ short spec))



labelled :: String -> [Opt] -> Maybe Args
labelled l rs = case filter (\r -> label r == l) rs of
  []    -> Nothing
  (x:_) -> Just (args x)

labelledV :: String -> [Opt] -> Maybe String
labelledV l rs = do
  xs <- labelled l rs
  case xs of
    OneArg v -> return v
    _ -> Nothing

labelledV2 :: String -> [Opt] -> Maybe (String,String)
labelledV2 l rs = do
  xs <- labelled l rs
  case xs of
    TwoArg a b -> return (a,b)
    _ -> Nothing

present :: String -> [Opt] -> Bool
present l rs = not . null $ filter (\r -> label r == l) rs
