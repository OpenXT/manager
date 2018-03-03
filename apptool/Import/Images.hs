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

{-# LANGUAGE PatternGuards #-}
module Import.Images
       (
         DIM
       , ImportedDiskImage (..)
       , importDiskImages
       , writeDIM
       , readDIM
       , getDiskImportedPhysPath
       , getDiskImportedKeyPath
       )
       where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import qualified Control.Exception as E
import Control.Concurrent
import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy as BL
import Text.Printf

import System.FilePath
import System.IO
import System.Directory

import Tools.Process
import Tools.Misc
import Tools.Text

import Appliance
import VirtualSystem
import Import.Types
import Import.Monad
import Import.Files

import Util

type DIM = [(Maybe DiskID, ImportedDiskImage)]

data ImportedDiskImage
   = ImportedDiskImage
     { idiID :: DiskImageID
     , idiData :: IDIData }

data IDIData
   = IDIData { idiPhysPath :: FilePath
             , idiKeyPath :: Maybe FilePath }

idiPhysPath' = idiPhysPath . idiData
idiKeyPath'  = idiKeyPath  . idiData

serialiseDIMEntry :: (Maybe DiskID, ImportedDiskImage) -> String
serialiseDIMEntry (diskID, idi) =
  printf "%s=%s %s %s %s" (q idiIDStr) (q $ idiPhysPath' idi) (q . fromMaybe "" $ idiKeyPath' idi) (q sysid) (q index)
  where
    q x = replace " " "%20" x
    DiskImageID idiIDStr = idiID idi
    sysid = case diskID of
      Just (DiskID (VirtualSystemID str) _ ) -> str
      _ -> ""
    index = case diskID of
      Just (DiskID _ index) -> show index
      _ -> ""

deserialiseDIMEntry :: String -> Maybe (Maybe DiskID, ImportedDiskImage)
deserialiseDIMEntry str =
  case split '=' str of
    [k, v] -> case split ' ' v of
      [path, keypath, sysid, index] | not (null sysid)
                                    , not (null index) -> Just $
        ( Just (DiskID (VirtualSystemID (dq sysid)) (read (dq index)))
        , ImportedDiskImage (DiskImageID (dq k)) (IDIData (dq path) (mkKeyPath $ dq keypath)))
      [path, keypath, _, _ ] -> Just $
        ( Nothing
        , ImportedDiskImage (DiskImageID (dq k)) (IDIData (dq path) (mkKeyPath $ dq keypath)))
      _ -> Nothing
    _ -> Nothing
  where
   dq x = replace "%20" " " x
   mkKeyPath "" = Nothing
   mkKeyPath p  = Just p
   
        
writeDIM :: FilePath -> DIM -> IO ()
writeDIM path = writeFile path . unlines . map serialiseDIMEntry

readDIM :: FilePath -> IO DIM
readDIM path = catMaybes . map deserialiseDIMEntry . lines <$> readFile path

systems app = contentVirtualSystems $ appContent app

getImportedDiskImage :: Disk -> DIM -> Maybe ImportedDiskImage
getImportedDiskImage d dim = safeHead . map snd $ (filter match1 dim ++ filter match2 dim) where
  imageID d = diID `fmap` diskImage d

  imageMatches d imgid =
    case imageID d of
      Nothing  -> False
      Just iid -> iid == imgid
      
  match1 (Just diskid, idi) = diskid == diskID d && imageMatches d (idiID idi)
  match1 _ = False
  match2 (_, idi) = imageMatches d (idiID idi)
  
  safeHead (h:_) = Just h
  safeHead _ = Nothing
  
getDiskImportedPhysPath :: Disk -> DIM -> Maybe FilePath
getDiskImportedPhysPath d dim = idiPhysPath' <$> getImportedDiskImage d dim
  
getDiskImportedKeyPath :: Disk -> DIM -> Maybe FilePath
getDiskImportedKeyPath d dim = join (idiKeyPath' <$> getImportedDiskImage d dim)

sharedDiskImages :: [VirtualSystem] -> [DiskImage]
sharedDiskImages systems = nubBy (\a b -> diID a == diID b) . filter diShared $ vsImages where
  vsImages = concatMap vsImages' systems
  vsImages' = catMaybes . map diskImage . vsDisks where

instancedDiskImages :: [VirtualSystem] -> [(DiskID, DiskImage)]
instancedDiskImages systems = filter (not . diShared . snd) $ vsImages where
  vsImages = concatMap vsImages' systems
  vsImages' = catMaybes . map f . vsDisks where
    f disk = case diskImage disk of
      Just img -> Just (diskID disk, img)
      _ -> Nothing

importDiskImages :: App -> Import DIM
importDiskImages app = do
  let shared = sharedDiskImages $ systems app
      instanced = instancedDiskImages $ systems app
  
  shared_ <- zip (repeat Nothing) <$> mapM (importDiskImage (appID app)) shared
  instanced_ <- mapM importInstance instanced
  return (shared_ ++ instanced_)
  where
    importInstance (diskid, img) = do
      idi <- importDiskImage (appID app) img
      return (Just diskid, idi)

importDiskImage :: AppID -> DiskImage -> Import ImportedDiskImage
importDiskImage appid im = case diType im of
  VHD -> importDiskImageVHD appid im
  ISO -> importISO im
  RawFilesystem -> importRawFilesystem appid im
  CPIO -> importCPIO appid im
  
importISO :: DiskImage -> Import ImportedDiskImage
importISO img = ImportedDiskImage <$> pure (diID img) <*> imp where
  imp = case diFile img of
    Nothing -> return $ IDIData "/storage/null.iso" Nothing
    Just fr -> IDIData <$> importISOFile fr <*> pure Nothing

inform m = liftIO (hPutStrLn stderr m)

untemp :: IDIData -> Import IDIData
untemp (IDIData vhd (Just key)) = IDIData <$> untempFileName vhd <*> (Just <$> untempFileName key)
untemp (IDIData vhd Nothing)    = IDIData <$> untempFileName vhd <*> pure Nothing

importRawFilesystem :: AppID -> DiskImage -> Import ImportedDiskImage
importRawFilesystem appid img = ImportedDiskImage <$> pure (diID img) <*> imp where
  imp = do
    name <- nameVhdFor appid img
    case diFile img of
      Nothing -> throwError $ FilesystemImageFileNotSpecified (show $ diID img)
      Just fr -> do
        vhd <- createVhd name (fromIntegral capacityMBs)
        key <- setupEncryption vhd (diEncryption img)
        removeFileOnError vhd $ do
          src <- fileSrcPath <$> fileSourceFromR fr
          inform $ printf "copying filesystem image %s -> %s" (show fr) name
          withCryptoVhdTap vhd $ \dev -> do
            ddFile src dev
            sync
        untemp (IDIData vhd key)
  -- rounded to upper bound
  capacityMBs = round $ diCapacity img `divMod` (1024*1024) where round (x,y) = x + signum y
        
        
importDiskImageVHD :: AppID -> DiskImage -> Import ImportedDiskImage
importDiskImageVHD appid img = ImportedDiskImage <$> pure (diID img) <*> imp where
  imp :: Import IDIData
  imp = do
    name <- nameVhdFor appid img
    case diFile img of
      Nothing -> do
        vhd <- createVhd name (fromIntegral capacityMBs)
        key <- setupEncryption vhd (diEncryption img)
        untemp (IDIData vhd key)
      Just fr -> do
        vhd <- tempFileName =<< importVHDFile name fr
        key <- setupEncryption vhd (diEncryption img)
        untemp (IDIData vhd key)
    
  -- rounded to upper bound
  capacityMBs = round $ diCapacity img `divMod` (1024*1024) where round (x,y) = x + signum y
                           
importCPIO :: AppID -> DiskImage -> Import ImportedDiskImage
importCPIO appid img = ImportedDiskImage <$> pure (diID img) <*> imp where
  imp = do
    name <- nameVhdFor appid img
    case diFile img of
      Nothing -> throwError $ CPIOImageFileNotSpecified (show $ diID img)
      Just fr -> do
        src <- fileSourceFromR fr
        -- create vhd, make filesystem on it, extract cpio archive
        vhd <- createVhd name (fromIntegral capacityMBs)
        key <- setupEncryption vhd (diEncryption img)
        removeFileOnError vhd $ do
          withCryptoVhdTap vhd $ \dev -> do
            mkfs (fromMaybe Ext3 (diFilesystem img)) dev
            withMountedDev dev $ \dst -> extractAppCpio src dst >> sync
        untemp (IDIData vhd key)

  -- rounded to upper bound
  capacityMBs = round $ diCapacity img `divMod` (1024*1024) where round (x,y) = x + signum y

-- figure out basename for vhd image, possibly appending instance uuid if not shared
nameVhdFor :: AppID -> DiskImage -> Import String
nameVhdFor (AppID appid appver) img = do
  instanceID <- if diShared img then pure Nothing else Just . show <$> liftIO uuidGen
  return (diskname instanceID)
  where
    diskname instanceID = appid ++ "-" ++ imgid ++ instid ++ ".vhd" where
      DiskImageID imgid = diID img
      instid = case instanceID of
        Nothing -> ""
        Just s  -> '-':s

createVhd :: FilePath -> Int -> Import FilePath
createVhd filename sizeMB = do
  path <- tempFileName =<< ((</> filename) <$> diskFolder)
  addImportFile path
  inform ("creating VHD " ++ path ++ " capacity=" ++ show sizeMB ++ " MB")
  liftIO $ createDirectoryIfMissing True (takeDirectory path)  
  _ <- liftIO $ readProcessOrDie "vhd-util" ["create", "-s", show sizeMB, "-n", path] ""
  return path

allocatedBlockCount :: FilePath -> IO Int
allocatedBlockCount vhd = do
  countStr <- liftIO $ readProcessOrDie "vhd-util" ["query", "-a", "-n", vhd] ""
  return $ fromMaybe 0 $ maybeRead countStr
  
setupEncryption :: FilePath -> DiskEncryption -> Import (Maybe FilePath)
setupEncryption vhd e = case e of
  NoEncryption -> return Nothing
  (GenerateCryptoKey bits) -> do
    key <- createKeyFile bits vhd
    -- set the key on vhd
    blocks <- liftIO $ allocatedBlockCount vhd
    when (blocks == 0) $
      void $ liftIO $ readProcessOrDie "vhd-util" ["key", "-s", "-n", vhd, "-k", key] ""
    return (Just key)
  (UseCryptoKey fileRes) -> do
    key <- tempFileName =<< importEncryptionKeyFile vhd fileRes
    -- set the key on vhd
    blocks <- liftIO $ allocatedBlockCount vhd
    when (blocks == 0) $
      void $ liftIO $ readProcessOrDie "vhd-util" ["key", "-s", "-n", vhd, "-k", key] ""
    return (Just key)
  where
      valid_keysizes = [ 256, 512 ]
      keyPath vhd bits = do
        dir <- cryptoKeysFolder
        return $ dir </> encryptionKeyFileName vhd bits

      copy s d n = liftIO (BL.readFile s >>= return . BL.take n >>= BL.writeFile d)
      
      createKeyFile bits vhd = do
        inform $ "... generating encryption key of size " ++ show bits ++ ", please move mouse to fill system entropy buffer faster"
        let src = "/dev/random"
        dst <- tempFileName =<< keyPath vhd bits
        addImportFile dst
        copy src dst (fromIntegral $ bits `div` 8) >> return dst
        return dst
      
tapCreate ty extraEnv path = do
  exist <- doesFileExist path
  when (not exist) $ error ("file " ++ show path ++ "does not exist")
  chomp <$> readProcessOrDieWithEnv extraEnv "tap-ctl" ["create", "-a", ty++":"++path] ""
tapCreateVhd = tapCreate "vhd"
tapDestroy dev = void $ readProcessOrDie "tap-ctl" ["destroy","-d",dev] ""

finally' = flip E.finally

withCryptoVhdTap :: FilePath -> (FilePath -> IO a) -> Import a
withCryptoVhdTap vhdfile action = do
  keys <- tempFileName =<< cryptoKeysFolder
  liftIO $ withVhdTap [("TAPDISK2_CRYPTO_KEYDIR", keys), ("TAPDISK3_CRYPTO_KEYDIR", keys)] vhdfile action

withVhdTap :: [(String,String)] -> FilePath -> (FilePath -> IO a) -> IO a
withVhdTap extraenv vhdfile action = E.bracket (tapCreateVhd extraenv vhdfile) tapDestroy action
  
sync = void $ readProcessOrDie "sync" [] ""

mkfs :: FilesystemType -> FilePath -> IO ()
mkfs fs dev = do
  let (cmd,args) | fs == Swap = (                     "mkswap",  [dev])
                 | otherwise  = (("mkfs." ++ filesystemStr fs),  [dev])
      
  inform (cmd ++ " " ++ intercalate " " args)
  void $ readProcessOrDie cmd args ""
  when (fs `elem` [Ext2, Ext3, Ext4]) $ tunefs dev
  where tunefs dev = void $ readProcessOrDie "tune2fs" ["-i", "0", "-c", "-1", "-m", "0", dev] ""

mount :: FilePath -> FilePath -> IO ()
mount dev dir = void $ readProcessOrDie "mount" [dev, dir] ""

umount :: FilePath -> IO ()
umount dir = void $ readProcessOrDie "umount" [dir] ""

withMountedDev :: FilePath -> (FilePath -> IO a) -> IO a
withMountedDev dev action =
  withTempDirectory $ \tempdir -> do
    mount dev tempdir
    action tempdir `E.finally` umount tempdir
    
removeFileOnError :: FilePath -> Import a -> Import a
removeFileOnError file f =
  f `catchError` (\err -> liftIO (removeFile file) >> throwError err)
