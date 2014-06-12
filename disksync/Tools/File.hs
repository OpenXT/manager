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

-- Some utility file functions
module Tools.File (
              maybeGetContents
            , filesInDir
            , module Tools.FileC
            , doesFileExist
            ) where

import Control.Monad
import Directory
import System.IO
import System.FilePath.Posix
import Tools.FileC

maybeGetContents :: FilePath -> IO (Maybe String)
maybeGetContents p = doesFileExist p >>= maybeRead
          where maybeRead False = return Nothing
                maybeRead True  = readFile p >>= return . Just


-- Get filepaths in directory, without . or ..
filesInDir :: FilePath -> IO [FilePath]
filesInDir p = do
    allentries <- getDirectoryContents p
    let entries = filter interesting allentries
    return $ map addPrefix entries
  where
    interesting e = e /= "." && e /= ".."
    addPrefix e   = joinPath [p, e]

