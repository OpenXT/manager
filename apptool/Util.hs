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

module Util
       (
         withTempDirectory
       , rmDirRec
       , cpDirRec
       , forEveryDirFileRec
       , mvFile         
       , finallyCME
       ) where

import qualified Control.Exception as E
import Control.Monad.Error
import Control.Applicative
import System.Directory
import System.FilePath
import System.Posix.Files
import Core.Uuid
import Tools.Process
import Tools.Misc
import Tools.File

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = do
  systemTemp <- getTemporaryDirectory
  uuid <- uuidGen
  let name = systemTemp </> show uuid
  createDirectory name
  action name `E.finally` (rm name)
  where
    rm name = readProcessOrDie "rm" ["-rf", name] ""

rmDirRec :: FilePath -> IO ()
rmDirRec p = void$ readProcessOrDie "rm" ["-rf", p] ""

cpDirRec :: FilePath -> FilePath -> IO ()
cpDirRec from to = void$ readProcessOrDie "cp" ["-r", from, to] ""

mvFile :: FilePath -> FilePath -> IO ()
mvFile from to = void$ readProcessOrDie "mv" [from, to] ""

getDirFilesRec :: FilePath -> IO [FilePath]
getDirFilesRec folder = go folder where
  go folder = do
    stat <- getFileStatus folder
    case () of
      _ | isDirectory stat -> concat <$> (mapM go =<< contents)
        | otherwise -> return [folder]
    where
      contents = map (\f -> folder ++ "/" ++ f) <$> getDirectoryContents_nonDotted folder
  
forEveryDirFileRec :: FilePath -> (FilePath -> IO ()) -> IO ()
forEveryDirFileRec folder f = mapM_ f =<< getDirFilesRec folder

finallyCME :: MonadError e m => m () -> m a -> m a
finallyCME fin f = do
  r <- f `catchError` (\e -> fin >> throwError e)
  fin
  return r
