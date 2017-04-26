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

{-# LANGUAGE GeneralizedNewtypeDeriving,PatternGuards,FlexibleInstances,TypeSynonymInstances #-}

module UpgradeEngine ( module JSONTrees
                     , Migration (..)
                     , runMigrate
                     , getCurrentVersion

                       -- transform a given json file using specified function
                     , xformJSONFile
                       -- transform a primary json file using specified function
                     , xformPrimaryJSON
                       -- transform all VM json files using specified function
                     , xformVmJSON
                       -- transform all dom-store json files using specified function
                     , xformDomStoreJSON ) where

import Data.String
import Data.List
import Control.Monad
import Control.Monad.State
import System.IO
import qualified System.IO.UTF8 as UTF8
import System.FilePath
import ShellTools
import JSONTrees

mainConfigFile :: String
mainConfigFile = "/config/db"

vmsDirectory :: String
vmsDirectory = "/config/vms"

domStoreDirectory :: String
domStoreDirectory = "/config/dom-store"

-- describe a single migration
data Migration = Migration {
      sourceVersion :: Int
    , targetVersion :: Int
    , actions :: IO ()
    }

runMigrate :: Migration -> IO ()
runMigrate m = do
    v <- getCurrentVersion
    when (v /= sourceVersion m) $
         error $ "unexpected DB version, expecting " ++ show (sourceVersion m) ++ ", is " ++ show v
    actions m
    -- overwrite with new DB version
    setCurrentVersion (targetVersion m)

getCurrentVersion :: IO Int
getCurrentVersion = do
  config <- configJSON
  let (Just versionStr) = fmap jsUnboxString $ jsGet "/db-version" config
  return $ read versionStr

setCurrentVersion :: Int -> IO ()
setCurrentVersion v = do
  xformPrimaryJSON $ \config ->
      jsSet "/db-version" (jsBoxString . show $ v) config

readJSONFile :: FilePath -> IO JSValue
readJSONFile path = do
  contents <- UTF8.readFile path
  case decodeStrict $ contents of
    Error msg -> error $ "malformed JSON file: " ++ msg
    Ok json   -> return json

configJSON :: IO JSValue
configJSON = readJSONFile mainConfigFile

type JSXform = JSValue -> JSValue

-- apply a transformation on given json file
xformJSONFile :: FilePath -> JSXform -> IO ()
xformJSONFile path xform = do
  json <- readJSONFile path
  let json' = xform json
  writeFile path (jsPretty json')

-- apply a transformation on primary config file
xformPrimaryJSON :: JSXform -> IO ()
xformPrimaryJSON xform = xformJSONFile mainConfigFile xform

-- apply a transformation on all vm json files
xformVmJSON :: JSXform -> IO ()
xformVmJSON = xformDirectory vmsDirectory

-- apply a transformation on all dom-store files
xformDomStoreJSON :: JSXform -> IO ()
xformDomStoreJSON = xformDirectory domStoreDirectory

-- apply xform to all .db files in the given directory
xformDirectory :: String -> JSXform -> IO ()
xformDirectory directory xform = do
  files <- directoryDbFiles directory
  mapM_ (`xformJSONFile` xform) files

-- get all .db files in the given directory
directoryDbFiles :: String -> IO [FilePath]
directoryDbFiles directory = do
  output <-spawnShell' $ "ls -1 " ++ combine directory "*.db"
  let files = case output of
                Nothing -> []
                Just v  -> lines v
  return $ map (combine directory) files
