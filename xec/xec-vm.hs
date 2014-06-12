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

{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Monad
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Ord
import Data.List
import Data.String
import System.Process
import System.Posix.Process
import System.Exit
import System.Environment
import Text.Printf

import Network.DBus

import Option
import Show

data Vm = Vm { vmPath :: String }
data VmInfo = VmInfo { iDomid :: Int, iName :: String, iUuid :: String, iState :: String }

specs :: [OptSpec]
specs =
  [
    OptSpec "help" "h" noArg
  , OptSpec "query" "q" noArg
  , OptSpec "session" "" noArg
  , OptSpec "list-interfaces" "" noArg
  , OptSpec "list-methods" "" noArg
  , OptSpec "list-properties" "" noArg
  , OptSpec "list-disks" "" noArg
  , OptSpec "list-nics" "" noArg
  , OptSpec "disk" "k" oneArg
  , OptSpec "nic" "c" oneArg
  , OptSpec "name" "n" oneArg
  , OptSpec "uuid" "u" oneArg
  , OptSpec "domid" "d" oneArg
  , OptSpec "service" "s" oneArg
  , OptSpec "object" "o" oneArg
  , OptSpec "interface" "i" oneArg
  , OptSpec "get" "g" oneArg
  , OptSpec "set" "x" twoArg
  ]

main = do
  args <- getArgs
  case parse specs args of
    Nothing -> return ()
    Just (opts, posargs) -> withConnection $ \c -> do
      when ( present "help" opts) $ putStrLn usageStr >> exitSuccess
      vm <- selectVm c opts
      case vm of
        Nothing -> listVms >>= pprintVms c
        Just vm -> case () of
          _ | present "list-disks" opts -> listDisks vm
            | present "list-nics" opts -> listNics vm
            | Just v <- labelledV "disk" opts -> xec_ ("-o":(vmPath vm++"/disk/"++v):chop_opts args)
            | Just v <- labelledV "nic" opts -> xec_ ("-o":(vmPath vm++"/nic/"++v):chop_opts args)
            | otherwise -> xec_ ("-o":vmPath vm:chop_opts args)

listVms = map Vm . lines <$> xec ["list-vms"]
pprintVms c vms = do
  putStrLn $ printf " %-3s | %-18s | %-36s | %-10s" "ID" "Name" "UUID" "State"
  putStrLn $        "-----+--------------------+--------------------------------------+--------"
  mapM_ pprint =<< return . sortVms =<< mapM info vms
  where
    info vm = VmInfo <$> (read <$> domid c vm) <*> name c vm <*> uuid c vm <*> state c vm
    sortVms = sortBy (comparing getDomid) where getDomid vm | iDomid vm < 0 = 100000
                                                            | otherwise = iDomid vm
    pprint vm = do
      let domid_str = if iDomid vm < 0 then "" else show (iDomid vm)
      putStrLn $ printf " %3s | %-18s | %-36s | %-10s" domid_str (iName vm) (iUuid vm) (iState vm)

listDisks vm = xec_ ["-o", vmPath vm, "list-disks"]
listNics  vm = xec_ ["-o", vmPath vm, "list-nics"]

selectVm c opts
  = case () of
    _ | Just v <- opt "object" -> return . Just $ Vm v
      | Just v <- opt "name"   -> Just . fromMaybe (error $ printf "vm '%s' not found" v) <$> byName c v
      | Just v <- opt "uuid"   -> Just . fromMaybe (error $ printf "vm '%s' not found" v) <$> byUuid c v
      | Just v <- opt "domid"  -> Just . fromMaybe (error $ printf "vm '%s' not found" v) <$> byDomid c v
      | otherwise -> return Nothing
    where
      opt x = labelledV x opts

byProperty pro c value
  = return . headS =<< filterM satisfied =<< listVms
  where
    satisfied vm = (== value) <$> pro c vm
    headS (x:xs) = Just x
    headS _ = Nothing
byName  = byProperty name
byUuid  = byProperty uuid
byDomid = byProperty domid

name  = property "name"
uuid  = property "uuid"
domid = property "domid"
state = property "state"

property pro c vm =
  return . rval =<< ( call_ c (fromString "com.citrix.xenclient.xenmgr") $
    DBusCall (fromString $ vmPath vm) "Get" (Just "org.freedesktop.DBus.Properties") [ str "com.citrix.xenclient.xenmgr.vm", str pro ] )
  where
    rval r = case returnBody r of
      [ v ] -> showDBus v
      _ -> error "property: rpc returned invalid value"

xec :: [String] -> IO String
xec args = from =<< readProcessWithExitCode "xec" args "" where
  from (ExitSuccess, out, err) = return out
  from (ExitFailure _, out, err) = error err

xec_ :: [String] -> IO ()
xec_ args = executeFile "xec" True args Nothing

chomp = reverse . dropWhile endl . reverse where endl = (`elem` ['\n', '\r'])
call_ c n b = call c n b `E.catch` err where
  err :: DBusError -> IO a
  err x = do putStrLn $ printf "%s" (errorName x)
             exitWith (ExitFailure 1)
str = DBusString . PackedString . C.pack
withConnection :: (DBusConnection -> IO a) -> IO a
withConnection f = f =<< establish busGetSystem authenticateWithRealUID

chop_opts = go where
  go [] = []
  go (x:_:xs) | x `elem` chopped = go xs
  go (x:xs) = x : go xs
  chopped = [
    "-n", "-u", "-d", "-o", "-k", "-c",
    "--name", "--uuid", "--domid", "--object", "--disk", "--nic" ]

usageStr = unlines [
  "Usage: xec-vm [options] [get/getall/set] ..."
 ,"Options:"
 ,"    -q, --query                      query mode, won't execute anything"
 ,"    -n, --name NAME                  select vm by name"
 ,"    -u, --uuid UUID                  select vm by uuid"
 ,"    -d, --domid DOMID                select vm by domid"
 ,"    -o, --path PATH                  select vm by object path"
 ,"    -k, --disk ID                    select vm disk"
 ,"    -c, --nic ID                     select vm nic"
 ,"    -i, --interface NAME             only search through specified interface methods/properties"
 ,"    -g, --get PROPERTY               get value(s) of property(ies)"
 ,"    -x, --set PROPERTY VALUE         set value of property"
 ,"        --list-disks"
 ,"                                     list vm disks"
 ,"        --list-nics"
 ,"                                     list vm nics"
 ,"        --list-interfaces"
 ,"                                     list interfaces"
 ,"        --list-methods"
 ,"                                     list methods"
 ,"        --list-properties"
 ,"                                     list properties"
 ,"    -h, --help                       display this message"
 ]
