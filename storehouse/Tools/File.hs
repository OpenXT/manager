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

-- Some utility file functions
module Tools.File
    ( maybeGetContents
    , filesInDir
    , getDirectoryContents_nonDotted
    , getDirectoryContents_extension
    , getDirectoryContentsUUIDFormated
    , module Tools.FileC
    ) where

import Control.Monad
import Control.Applicative
import Control.Exception
import System.Directory
import System.IO
import System.FilePath.Posix
import Tools.FileC
import Tools.Uuid
import Data.List (isSuffixOf)
import Prelude hiding (catch)

maybeGetContents :: FilePath -> IO (Maybe String)
maybeGetContents p = doesFileExist p >>= maybeRead
          where maybeRead False = return Nothing
                maybeRead True  = readFile p >>= return . Just

filesInDir :: FilePath -> IO [FilePath]
filesInDir p = map (p </>) <$> getDirectoryContents_nonDotted p

dotted :: FilePath -> Bool
dotted "." = True
dotted ".." = True
dotted _ = False

getDirectoryContents_nonDotted :: FilePath -> IO [FilePath]
getDirectoryContents_nonDotted = fmap (filter $ not . dotted) . getDirectoryContents

getDirectoryContents_extension :: FilePath -> String -> IO [String]
getDirectoryContents_extension dirPath ext = filter isVhd <$> getDirectoryContents dirPath
    where isVhd = isSuffixOf ".vhd"

getDirectoryContentsUUIDFormated :: FilePath -> String -> IO [Uuid a]
getDirectoryContentsUUIDFormated dirPath ext = catch (filterToUuid <$> getDirectoryContents dirPath) (exnTo (return []))
	where
		filterToUuid = map (uuidFromString . take 36) . filter isUuidDotExt
		isUuidDotExt s = (reverse $ take 4 $ reverse s) == ext && length s == 40

		exnTo :: a -> SomeException -> a
		exnTo defVal _ = defVal
