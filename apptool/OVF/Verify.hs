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

module OVF.Verify
       (
         missingLocalFiles
       , verifyManifestFile
       ) where

import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import Text.Printf

import Tools.File

import Core.Types
import OVF.Model
import OVF.Manifest

missingLocalFiles :: FilePath -> [FileRef] -> IO [FileRef]
missingLocalFiles ovfRoot = filterM missing where
  missing f = do
   case parseURI (fileHref f) of
     Nothing  -> return False
     Just uri -> case uriScheme uri of
         ""     -> not <$> doesFileExist (ovfRoot </> uriLocation uri)
         "file" -> not <$> doesFileExist (uriLocation uri)
         _      -> return False

verifyManifestFile :: FilePath -> FilePath -> [FileRef] -> IO ()
verifyManifestFile mfpath root refs = withManifest =<< (parseManifest <$> readFile mfpath) where
  withManifest (Left err) = error ("error in manifest file: " ++ show err)
  withManifest (Right mf) = mapM_ verifyFileDigest mf
  
  verifyFileDigest (FileDigest fname alg csum) = do
    let f = root </> fname
    hPutStrLn stderr $ "verification of checksum for " ++ fname
    
    csum' <- case alg of
      Sha1 -> fileSha1Sum f
      Sha256 -> fileSha256Sum f
    when (csum /= csum') $
      let showcs c = printf "%0x" c in
      error ("mismatched checksum for " ++ show fname ++ " expected " ++ (showcs csum) ++ " have " ++ (showcs csum'))
