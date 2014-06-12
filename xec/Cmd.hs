--
-- Copyright (c) 2013 Citrix Systems, Inc.
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

module Cmd where

import Data.Maybe
import Control.Applicative
import Control.Monad
import Option

data Location
   = Location { service :: String, object :: String, interface :: Maybe String }
     deriving ( Show )

data Flags
   = Flags { query :: Bool
           , session :: Bool
           , domid :: Int }
     deriving ( Show )

type PropertyName = String
type MethodName = String
type Value = String

data Cmd = Cmd Flags Cmd_ deriving ( Show )
data Cmd_
   = CmdGet Location PropertyName
   | CmdGetAll Location
   | CmdSet Location (PropertyName,Value)
   | CmdCall Location MethodName [Value]
   | CmdListInterfaces Location
   | CmdListMethods Location
   | CmdListProperties Location
   | CmdIntrospect Location
   | CmdHelp
     deriving ( Show )

getCmd :: [String] -> Maybe Cmd
getCmd args = fmap normalise $
  case parse specs args of
    Nothing -> Nothing
    Just (opts, posargs) -> do
      let session = present "session" opts
          query   = present "query" opts
          help    = present "help" opts
          flags   = Flags query session domid
          domid   = fromMaybe (-1) $ join . fmap readI $ labelledV "target-domid" opts
          readI s = case reads s of 
            ((v,_):_) -> Just v
            _ -> Nothing
          service  = fromMaybe "com.citrix.xenclient.xenmgr" $ labelledV "service" opts
          object   = fromMaybe "/" $ labelledV "object" opts
          location = Location service object (labelledV "interface" opts)

          get = labelledV  "get" opts
          set = labelledV2 "set" opts

          others = case posargs of
              ("getall" : _)      -> Just . Cmd flags $ CmdGetAll location
              ("get" : p : _)     -> Just . Cmd flags $ CmdGet location p
              ("set" : p : v : _) -> Just . Cmd flags $ CmdSet location (p,v)
              (method : args)     -> Just . Cmd flags $ CmdCall location method args
              []                  -> Just . Cmd flags $ CmdIntrospect location

      boolM help (Cmd flags CmdHelp)
        `mplus` boolM (present "list-interfaces" opts) (Cmd flags $ CmdListInterfaces location)
        `mplus` boolM (present "list-methods" opts) (Cmd flags $ CmdListMethods location)
        `mplus` boolM (present "list-properties" opts) (Cmd flags $ CmdListProperties location)
        `mplus` ((\p -> Cmd flags $ CmdGet location p) `fmap` get)
        `mplus` ((\(p,v) -> Cmd flags $ CmdSet location (p,v)) `fmap` set)
        `mplus` others

  where
    normalise (Cmd flags (CmdCall location method args)) = Cmd flags (CmdCall location (normalise_method method) args)
    normalise x = x
    normalise_method = replace '-' '_'

boolM :: Bool -> a -> Maybe a
boolM True  x = Just x
boolM False _ = Nothing

replace p q [] = []
replace p q (x:xs) | x == p = q:replace p q xs
                   | otherwise = x:replace p q xs

specs :: [OptSpec]
specs =
  [
    OptSpec "help" "h" noArg
  , OptSpec "query" "q" noArg
  , OptSpec "session" "" noArg
  , OptSpec "list-interfaces" "" noArg
  , OptSpec "list-methods" "" noArg
  , OptSpec "list-properties" "" noArg
  , OptSpec "service" "s" oneArg
  , OptSpec "object" "o" oneArg
  , OptSpec "interface" "i" oneArg
  , OptSpec "get" "g" oneArg
  , OptSpec "set" "x" twoArg
  , OptSpec "target-domid" "t" oneArg
  ]
{-
optparse cmdP ["-o", "/", "-s", "com.citrix.xenclient.foodaemon", "-i", "foodaemon.interface", "-g", "property"]
optparse cmdP ["-q", "-o", "/", "-s", "com.citrix.xenclient.foodaemon", "-i", "foodaemon.interface", "-x", "property", "value"]
optparse cmdP ["-x", "-i", "-v", "value", "-o", "/", "-s", "com.citrix.xenclient.foodaemon", "-i", "foodaemon.interface"]
optparse cmdP ["-o", "/", "-s", "com.citrix.xenclient.foodaemon", "-i", "-x", "-x", "property", "value"]
optparse cmdP ["-o", "/", "-s", "com.citrix.xenclient.foodaemon", "-i", "-x", "-x", "property", "value"]
optparse cmdP ["-o", "/", "-s", "com.citrix.xenclient.foodaemon", "get", "property"]
optparse cmdP ["-o", "/", "-s", "com.citrix.xenclient.foodaemon", "set", "property", "value"]
optparse cmdP ["-o", "/", "-s", "com.citrix.xenclient.foodaemon", "switch", "0", "ala", "ma"]
optparse cmdP ["-o", "/", "-s", "com.citrix.xenclient.foodaemon", "switch"]
optparse cmdP ["-o", "/", "-x", "property", "-v", "value"]
-}
