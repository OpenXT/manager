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

module Import.Files
       (
         importVHDFile
       , importDomStoreFile
       , importEnvFile
       , importISOFile
       , importEncryptionKeyFile
       , copyAppFile
       , copyAppFile'
       , getAppFileSize
       , extractAppCpio
       , diskFolder
       , fileSourceFromR
       , fileSrcPath
       , encryptionKeyFileName
       , cryptoKeysFolder
       , applianceTempFolder
       , tempFileName
       , untempFileName
       , ddFile
       ) where
       
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Data.List
import System.Directory hiding (copyFile)
import System.FilePath hiding ( (</>) )
import System.Posix
import System.IO
import Text.Printf

import Tools.Process
import Tools.IfM

import Core.Types

import Import.Types
import Import.Monad
import AppInventory
import Appliance

data FileSource
   = FileSourceHost FilePath -- path to file on host
   | FileSourceOvf FilePath  -- path to file in ovf package
   | FileSourceHttp URI      -- path to file on http server

fileSrcPath (FileSourceHost p) = p
fileSrcPath (FileSourceOvf p) = p
fileSrcPath (FileSourceHttp uri) = show uri

inform m = liftIO (hPutStrLn stderr m)

-- </> variant which joins /foo and /bar as /foo/bar instead /bar
infixr 5 </>
(</>) "" b = b
(</>) a b = reverse (dropWhile (== '/') (reverse a)) ++ "/" ++ dropWhile (== '/') b
  
onHost :: FileSource -> Bool
onHost (FileSourceHost _) = True
onHost _ = False

encryptionKeyFileName :: String -> Int -> String
encryptionKeyFileName vhdpath sz = takeBaseName vhdpath ++ ",aes-xts-plain,"++show sz++".key"

-- all files first go to this temporary folder
applianceTempFolder :: Import FilePath
applianceTempFolder = do
  AppID name _ <- app
  return $ "/storage/_apptemp_/" ++ (untemp name)
  where
    untemp name | "_temp_" `isPrefixOf` name = drop (length "_temp_") name
                | otherwise = name

-- version of the filename in the temporary folder
tempFileName :: FilePath -> Import FilePath
tempFileName p = do
  temp <- applianceTempFolder
  case () of
    _ | temp `isPrefixOf` p -> return p
      | otherwise -> return (temp </> p)

-- convert temp file name to real file name
untempFileName :: FilePath -> Import FilePath
untempFileName p = do
  temp <- applianceTempFolder
  case () of
    _ | temp `isPrefixOf` p -> return $ drop (length temp) p
      | otherwise -> return p

-- all file copies go into temp folder only and get relinked at late import stage
-- DD seems to be needed in preference to System.Directory.copyFile
-- otherwise, tapdisk errors happen
copyFile src dst = do
  temp <- applianceTempFolder
  let dst' = temp </> dst
  liftIO $ createDirectoryIfMissing True (takeDirectory dst')
  liftIO $ ddFile src dst'

ddFile src dst = do
  inform $ "copy " ++ src ++ " -> " ++ dst
  readProcessOrDie "dd" ["bs=32768", "if="++src, "of="++dst] ""

-- copy file out of appliance repo or host location
copyAppFile :: FileResource -> FilePath -> Import FilePath
copyAppFile src dst = do
  src' <- fileSourceFromR src
  copyAppFile' src' dst
  
fileSz :: FilePath -> IO FileOffset
fileSz path = fileSize <$> getFileStatus path

getAppFileSize :: FileSource -> Import FileOffset
getAppFileSize (FileSourceOvf p)  = liftIO $ fileSz p
getAppFileSize (FileSourceHost p) = liftIO $ fileSz p
getAppFileSize (FileSourceHttp uri) = liftIO $ error ("fetching files over http not yet supported: " ++ show uri)

copyAppFile' :: FileSource -> FilePath -> Import FilePath
copyAppFile' (FileSourceOvf src)  dst = copyFile src dst >> return dst
copyAppFile' (FileSourceHost src) dst = copyFile src dst >> return dst
copyAppFile' (FileSourceHttp uri) _   = liftIO $ error ("fetching files over http not yet supported: " ++ show uri)

-- copy file out of appliance repo but not out of host location
copyNonHostAppFile' :: FileSource -> FilePath -> Import FilePath
copyNonHostAppFile' (FileSourceOvf src)  dst  = copyFile src dst >> return dst
copyNonHostAppFile' (FileSourceHost src) dst  = return src
copyNonHostAppFile' (FileSourceHttp uri) _    = liftIO $ error ("fetching files over http not yet supported: " ++ show uri)

extractAppCpio :: MonadIO m => FileSource -> FilePath -> m ()
extractAppCpio (FileSourceOvf src)  dst = liftIO $ cpioExtract src dst
extractAppCpio (FileSourceHost src) dst = liftIO $ error ("CPIO archives must be referenced using OVF relative path")
extractAppCpio (FileSourceHttp uri) _   = liftIO $ error ("fetching files over http not yet supported: " ++ show uri)

cpioExtract src dst = do
  inform $ "extracting CPIO " ++ src ++ " -> " ++ dst
  mapM_ validateChar src
  src' <- canonicalizePath src
  safeSpawnShell $ printf "cd %s && (bzcat %s | cpio -idm)" dst src'
  return ()
  where
    validateChar ch | ch `elem` [';'] = error ("bad character in " ++ show src)
                    | otherwise = return ()

importDomStoreFile :: Uuid -> DomStoreFile -> Import FilePath
importDomStoreFile vm (DomStoreFile file path_in_domstore) = do
  src <- fileSourceFromR file
  -- dom store files are owned by vm, no need to acquire file ownership here
  domstoreDir <- domStoreFolder
  let dirp = domstoreDir </> show vm
      dst  = (dirp </> path_in_domstore </> takeFileName (fileSrcPath src))
  --inform $ printf "importing domstore file %s -> %s" (show file) dst
  appid <- app
  registerApplianceOwnedFile appid dst
  addImportFile dst
  copyAppFile' src dst

importEnvFile :: Uuid -> EnvFile -> Import FilePath
importEnvFile vm (EnvFile file relpath) = do
  src <- fileSourceFromR file
  appid <- app
  dir <- (</> show vm </> takeDirectory relpath) <$> ovffilesFolder
  let dst = dir </> takeFileName relpath
  --inform $ printf "importing env file %s -> %s" (show file) dst
  registerApplianceOwnedFile appid dst
  addImportFile dst
  copyAppFile' src dst

importISOFile :: FileResource -> Import FilePath
importISOFile fr = do
  f <- fileSourceFromR fr
  path <- (</> takeFileName (fileSrcPath f)) <$> isoFolder
  -- inform $ printf "importing ISO file %s" (show fr)
  when (not $ onHost f) $ addImportFile path
  copyNonHostAppFile' f path

importEncryptionKeyFile :: FilePath -> FileResource -> Import FilePath
importEncryptionKeyFile vhdpath fr = do
  src <- fileSourceFromR fr
  file_size <- getAppFileSize src
  let key_size = file_size * 8
  when (not $ key_size `elem` valid_keysizes) $ throwError $ InvalidEncryptionKeySize (fileSrcPath src)
  keyDir <- cryptoKeysFolder
  let dst = keyDir </> encryptionKeyFileName vhdpath (fromIntegral key_size)
  whenM (liftIO $ doesFileExist dst) $ throwError (EncryptionKeyAlreadyExists dst)
  appid <- app
  --registerApplianceOwnedFile appid dst
  addImportFile dst
  copyAppFile' src dst
  where
    valid_keysizes = [ 256, 512 ]

importVHDFile :: String -> FileResource -> Import FilePath
importVHDFile basename fr = do
  f <- fileSourceFromR fr
  path <- (</> basename) <$> diskFolder
  --inform $ printf "importing VHD image %s -> %s" (show fr) path
  addImportFile path
  copyAppFile' f path
  return path

fileSourceFromR :: FileResource -> Import FileSource
fileSourceFromR (FileResource uri) =
  case uriScheme uri of
    ""      -> FileSourceOvf <$> relativeAppFile (uriLocation uri)
    "file"  -> return $ FileSourceHost (uriLocation uri)
    "http"  -> return $ FileSourceHttp uri
    "https" -> return $ FileSourceHttp uri
    _       -> throwError (UnsupportedFileURL $ show uri)

relativeAppFile :: FilePath -> Import FilePath
relativeAppFile relpath = do
  root <- ovfRootPath <$> importContext
  let f = root </> relpath
  assertHasFile f
  return f

assertHasFile :: FilePath -> Import ()
assertHasFile p = exists =<< liftIO (doesFileExist p) where
  exists False = throwError (FileDoesNotExist p)
  exists _ = return()

diskFolder = importDiskFolder <$> options
isoFolder = importIsoFolder <$> options
domStoreFolder = importDomStoreFolder <$> options
cryptoKeysFolder = importCryptoKeysFolder <$> options
ovffilesFolder = importOvfFilesFolder <$> options
