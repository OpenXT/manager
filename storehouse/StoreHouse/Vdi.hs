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

module StoreHouse.Vdi
	( vdiFileRead
	, vdiFileExist
	, vdiFileWrite
	, vdiList
	, vdiFileDelete
	) where

import System.Directory
import Tools.Uuid
import Control.Applicative ((<$>))
import Control.Exception
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as B
import Prelude hiding (catch)

vdiPath :: FilePath -> Uuid KindVDI -> FilePath
vdiPath dirPath uuid = dirPath ++ show uuid ++ ".vdi"

vdiFileExist :: FilePath -> Uuid KindVDI -> IO Bool
vdiFileExist dirPath uuid = doesFileExist $ vdiPath dirPath uuid

vdiFileDelete :: FilePath -> Uuid KindVDI -> IO ()
vdiFileDelete dirPath uuid = removeFile $ vdiPath dirPath uuid

vdiFileRead :: FilePath -> Uuid KindVDI -> IO (Maybe FilePath)
vdiFileRead dirPath uuid = do
	either (exnTo Nothing) (Just . head . lines . UTF8.toString) <$> try (B.readFile $ vdiPath dirPath uuid)

vdiFileWrite :: FilePath -> Uuid KindVDI -> FilePath -> IO ()
vdiFileWrite dirPath uuid headFile = do
	createDirectoryIfMissing True dirPath
	writeFile (vdiPath dirPath uuid) headFile

vdiList :: FilePath -> IO [Uuid KindVDI]
vdiList dirPath = catch (filterToUuid <$> getDirectoryContents dirPath)
                        (exnTo (return []))
	where
		filterToUuid = map (uuidFromString . take 36) . filter isUuidDotVdi
		isUuidDotVdi s = (reverse $ take 4 $ reverse s) == ".vdi" && length s == 40

exnTo :: a -> SomeException -> a
exnTo defVal _ = defVal
