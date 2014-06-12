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
module Idl
       (
         IdlRepo
       , parseIDLRepository
       , vmIdl
       , diskIdl
       , nicIdl
       , findIDLProperty
       )
       where

import Control.Applicative
import Control.Monad (foldM)
import qualified Network.DBus.Introspect as I
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Map (Map)
import Data.Char
import Data.List (isPrefixOf)
import System.Directory
import System.FilePath
import Util

import Text.XML.HXT.Core
import Debug.Trace

data IdlRepo = IdlRepo {
    repo :: Map String I.Object
  , vmIdl :: I.Object
  , diskIdl :: I.Object
  , nicIdl :: I.Object
  }

-- FIXME: extend udbus-intro to handle the telepathy extensions
-- for now, scrub them out of the idls
-- FIXME: for some reason XML parser wants rw access to xmls, for now just copy them into temp folder and use in there
readIdlXml :: FilePath -> FilePath -> IO TL.Text
readIdlXml tempdir src = do
  let path = tempdir </> takeFileName src
  copyFile src path
  str <- head <$> runX ( readDocument xmlParseOpts path >>> processTopDown scrubA >>> writeDocumentToString [] )
  return $ TL.pack str
  where
    scrubA = none `when` (isTP <+> isSignal)
    isSignal = isElem >>> getName >>> isA (== "signal")
    isTP = isElem >>> getName >>> isA (isPrefixOf "tp:")
    xmlParseOpts
      = [ withValidate no
        , withCheckNamespaces no
        , withSubstDTDEntities no ]

parseIDLRepository :: FilePath -> IO IdlRepo
parseIDLRepository idlpath = withTempDirectory $ \idltmp ->
  make =<< mapM (parse idltmp) =<< enumerate where

    enumerate
      = filter (\f -> idl f && care f) <$> getDirectoryContents idlpath
        where idl = (== ".xml") . map toLower . takeExtension
              care = (`elem` ["xenmgr_vm.xml", "vm_disk.xml", "vm_nic.xml"])

    parse idltmp fname 
      = (,) fname . I.fromXml <$> readIdlXml idltmp path
        where
          basename = takeBaseName fname
          path = idlpath </> fname

    make objs
      = do mapping <- foldM f Map.empty objs
           vm   <- get "VM" "xenmgr_vm.xml" mapping
           disk <- get "Disk" "vm_disk.xml" mapping
           nic  <- get "NIC" "vm_nic.xml" mapping
           return $ IdlRepo mapping vm disk nic
        where f _ (fname, Left err_msg) = error $ "IDL parse error (" ++ show fname ++ "): " ++ show err_msg
              f m (fname, Right obj) = return $ Map.insert fname obj m
              get descr name mapping = from (Map.lookup name mapping) where
                from Nothing  = error $ "IDL file: " ++ name ++ " not found."
                from (Just o) = return o

findIDLProperty :: I.Object -> String -> [(I.Interface, I.Property)]
findIDLProperty idl name
  = concatMap scanInterface (I.objInterfaces idl)
    where
      name' = TL.pack . map toLower $ name
      scanInterface i
        | TL.null interface_name ||
          interface_name == I.intfName i
        = zip (repeat i) $ concatMap scanProperty (I.intfProperties i)

        | otherwise = []

      scanProperty p
        | property_name == I.propName p   = [p]
        | otherwise                       = []

      (property_name, interface_name) = (\(a,b) -> (TL.pack (reverse a), TL.pack $ reverse $ dropWhile (=='.') b)) . span (/= '.') . reverse $ name
