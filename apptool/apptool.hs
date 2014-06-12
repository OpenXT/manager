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
import Data.Char
import Data.Maybe
import Data.String
import Data.List

import System.Console.GetOpt
import System.Exit
import System.IO
import System.Environment
import System.FilePath
import System.Directory
import System.Process

import Text.Printf

import Tools.Process
import Tools.File
import Tools.IfM
import Tools.Text

import Rpc.Core

import Util
import Core.Types
import Rpc
import Appliance
import AppInventory
import ApplianceOVF
import Import
import Import.Types
import ParseDBus
import Idl
import VirtualSystem
import GenEnv
import qualified OVF.Model as Ovf
import qualified OVF.ModelXCI as Ovf
import qualified OVF.Parse as Ovf
import qualified OVF.Verify as Ovf

import Paths_apptool

defaultIdlDir = "/usr/share/idl"
defaultTempDir = "/storage/apptool"

data OptFlag
   = ImportApp FilePath
   | UpgradeApp
   | DowngradeApp
   | ReinstallApp
   | CleanupInv
   | RemoveApp String
   | VerifyApp FilePath
   | OnlyDiskImages
   | WithExistingDiskImages
   | DiskImageMap FilePath
   | GenEnvDoc String
   | GenEnvIso String
   | IsoFile FilePath
   | ListApp
   | SetAppId String
   | SetAppVer String
   | IDLDir FilePath
   | TempDir FilePath
   | Help
     deriving Eq

options :: [OptDescr OptFlag]
options =
  [ Option "" ["import"] (ReqArg ImportApp "FILE") "import appliance from FILE"
  , Option "" ["upgrade"] (NoArg UpgradeApp) "allow upgrade to be performed"
  , Option "" ["downgrade"] (NoArg DowngradeApp) "allow downgrade to be performed"
  , Option "" ["reinstall"] (NoArg ReinstallApp) "allow reinstall to be performed"
  , Option "" ["verify"] (ReqArg VerifyApp "FILE") "verify appliance FILE"
  , Option "" ["cleanup"] (NoArg CleanupInv) "cleanup appliance database, for example after interrupted import"
  , Option "" ["only-disk-images"] (NoArg OnlyDiskImages) "only import disk images"
  , Option "" ["with-existing-disk-images"] (NoArg WithExistingDiskImages) "use existing disk images"
  , Option "" ["disk-image-map"] (ReqArg DiskImageMap "FILE") "use/write disk image map FILE"
  , Option "" ["remove"] (ReqArg RemoveApp "ID") "remove appliance"
  , Option "" ["list"] (NoArg ListApp) "list installed appliances"
  , Option "" ["genenvdoc"] (ReqArg GenEnvDoc "UUID") "generate environment document for vm UUID"
  , Option "" ["genenviso"] (ReqArg GenEnvIso "UUID") "generate environment iso for vm UUID"
  , Option "" ["iso"] (ReqArg IsoFile "FILE") "iso output file"
  , Option "" ["set-id"] (ReqArg SetAppId "ID") "override appliance ID"
  , Option "" ["set-version"] (ReqArg SetAppVer "VERSION") "override appliance version"
  , Option "" ["idl"] (ReqArg IDLDir "DIR") "specify directory with xml IDL files"
  , Option "" ["temp"] (ReqArg TempDir "DIR") "specify directory used for temporary files"
  , Option "h" ["help"] (NoArg Help) "print usage information"
  ]

optImportPath :: [OptFlag] -> Maybe FilePath
optImportPath = foldr f Nothing where
  f (ImportApp p) r = Just p
  f _ r = r

optVerifyPath :: [OptFlag] -> Maybe FilePath
optVerifyPath = foldr f Nothing where
  f (VerifyApp p) r = Just p
  f _ r = r

optRemove :: [OptFlag] -> Maybe String
optRemove = foldr f Nothing where
  f (RemoveApp id) _ = Just id
  f _ r = r

optIdlDir :: [OptFlag] -> FilePath
optIdlDir = foldr f defaultIdlDir where
  f (IDLDir p) _ = p
  f _ p = p

optTempDir :: [OptFlag] -> FilePath
optTempDir = foldr f defaultTempDir where
  f (TempDir p) _ = p 
  f _ p = p

optAppId :: [OptFlag] -> Maybe String
optAppId = foldr f Nothing where
  f (SetAppId x) r = Just x
  f _ r = r
  
optAppVer :: [OptFlag] -> Maybe String
optAppVer = foldr f Nothing where
  f (SetAppVer x) r = Just x
  f _ r = r

optGenEnvDoc :: [OptFlag] -> Maybe Uuid
optGenEnvDoc = foldr f Nothing where
  f (GenEnvDoc x) r = Just (fromString x)
  f _ r = r

optGenEnvIso :: [OptFlag] -> Maybe Uuid
optGenEnvIso = foldr f Nothing where
  f (GenEnvIso x) r = Just (fromString x)
  f _ r = r

optIsoFile :: [OptFlag] -> Maybe FilePath
optIsoFile = foldr f Nothing where
  f (IsoFile x) r = Just x
  f _ r = r

optUpgrade :: [OptFlag] -> Bool
optUpgrade opts = UpgradeApp `elem` opts

optDowngrade :: [OptFlag] -> Bool
optDowngrade opts = DowngradeApp `elem` opts

optReinstall :: [OptFlag] -> Bool
optReinstall opts = ReinstallApp `elem` opts

optImportMode :: [OptFlag] -> ImportMode
optImportMode opts
  | OnlyDiskImages `elem` opts, Just p <- dim = ImportImagesOnly p
  | OnlyDiskImages `elem` opts = error "please specify path to disk image map file"
  | WithExistingDiskImages `elem` opts, Just p <- dim = ImportWithExistingImages p
  | WithExistingDiskImages `elem` opts = error "please specify path to disk image map file"
  | otherwise = ImportAll
  where
    dim = foldr f Nothing opts where f (DiskImageMap file) r = Just file
                                     f _ r = r

usage = do
  prg <- getProgName
  hPutStrLn stderr (usageInfo prg options)
  exitWith ExitSuccess

importOptions = 
  ImportOptions { 
      importDiskFolder = "/storage/disks"
    , importIsoFolder = "/storage/isos"
    , importOvfFilesFolder = "/storage/ovffiles"
    , importDomStoreFolder = "/config/dom-store"
    , importCryptoKeysFolder = "/config/platform-crypto-keys"
    , importMode = ImportAll
    , allowUpgrades = False
    , allowDowngrades = False
    , allowReinstall = False
    }

inform = hPutStrLn stderr

showAppInfo app = do
  inform $ show (appID app)
  mapM_ showSystemInfo (contentVirtualSystems $ appContent app)

showSystemInfo sys = do
  let VirtualSystemID idStr = vsID sys
  inform $ "virtual system id=" ++ idStr
  --inform (show sys)

verifyOVF root ovffile ovf =
  do missing <- Ovf.missingLocalFiles root (Ovf.references ovf)
     mapM_ report missing
     when (not . null $ missing) $ exitWith (ExitFailure 1)
     let mf_path = root </> takeBaseName ovffile ++ ".mf"
     whenM (liftIO $ doesFileExist mf_path) $ Ovf.verifyManifestFile mf_path root (Ovf.references ovf)
  where
    report f =
      hPutStrLn stderr $ printf "missing file: " ++ show (Ovf.fileHref f)

-- operation on unpacked ovf repo
withOvfRepo :: FilePath -> FilePath -> (FilePath -> FilePath -> IO a) -> IO a
withOvfRepo tempDir path f = test >> go where
  test = whenM (not <$> doesFileExist path) (error $ "file " ++ show path ++ " does not exist!")
  go = case ext of
    ".ovf" -> f (takeDirectory path) path -- inplace
    ".ova" -> do
      folder <- (\u -> tempDir </> "import-" ++ u) . show <$> uuidGen
      conjure folder
      E.finally (readProcessOrDie "tar" ["-xf", path, "-C", folder] "" >> (f folder =<< findOvf folder)) (dismiss folder)
    ext -> error $ "unknown file extension: " ++ ext
    
  ext = map toLower $ takeExtension path
  conjure folder = createDirectoryIfMissing True folder
  dismiss folder = readProcessOrDie "rm" ["-rf", folder] ""
  findOvf folder = do
      ovf <- filter isOvf <$> filesInDir folder
      case ovf of
        []  -> error "no .ovf file found inside .ova archive"
        [f] -> return f
        _   -> error ".ova archive incorrectly contains multiple .ovf files"
  isOvf = (== ".ovf") . takeExtension

schemaValidate xciovffile = do
  schemaDir <- getDataFileName "schema"
  let schemaFile = schemaDir </> "xciovf.xsd"
  (code, stdout, stderr) <- readProcessWithExitCode "xsd-validate" [schemaDir, "http://www.citrix.com/xenclient/ovf/1 "++schemaFile, xciovffile] ""
  case code of
    ExitSuccess -> return ()
    _ -> putStrLn stdout >> putStrLn stderr >> error "schema validation failed, aborting"

withConvertedAppliance :: [OptFlag] -> FilePath -> (FilePath -> (Ovf.Envelope,Ovf.XCIAppliance) -> App -> IO a) -> IO a
withConvertedAppliance opts path f = do
  let tempRoot = optTempDir opts
  withOvfRepo tempRoot path $ \ovfroot ovffile -> do
    schemaValidate ovffile
    idlrepo <- parseIDLRepository (optIdlDir opts)
    ovf <- Ovf.runParser ovffile
    case ovf of
      Nothing  -> error "OVF parser error"
      Just ovf -> do
        verifyOVF ovfroot ovffile (fst ovf)
        app <- overrideIDs opts <$> (conversionErrors =<< return (applianceFromOVF idlrepo ovf))
        f ovfroot ovf app
  
  where
    -- fixme for better parser error reporting
    conversionErrors (Left conv_err) = error $ "conversion error: " ++ show conv_err
    conversionErrors (Right envelope) = return envelope

    overrideIDs opts = ov_id . ov_ver where
      ov_id app | Just id' <- optAppId opts, AppID _ curVer <- appID app = app { appID = AppID id' curVer }
                | otherwise = app
      ov_ver app | Just ver' <- optAppVer opts, AppID curId _ <- appID app = app { appID = AppID curId (ver_int ver') }
                 | otherwise = app
        where ver_int ver = fromMaybe (error "specified version must be integer value") (maybeRead ver)

listApp :: Rpc ()
listApp = mapM_ (liftIO . putStrLn . showID) =<< enumAppliances
  where showID (AppID name ver) = name

removeApp :: String -> Rpc ()
removeApp strID = remove =<< findAppliance strID where
  remove Nothing = error $ "no such appliance: " ++ show strID
  remove (Just app) = unregisterAppliance app True True

importApp opts path = do
  inform "importing appliance..."
  withConvertedAppliance opts path $ \ovfroot ovf app -> do
    mapM_ acceptEula (Ovf.eulas $ fst ovf)
    importAppliance ovfroot imp_opts app
    inform "OK"
  where
    acceptEula eula = do
      inform eula
      inform "Do you agree to above license? (y/n)"
      ans <- getLine
      when (ans /= "y") $ error "license not accepted"
    
    imp_opts = importOptions { allowUpgrades   = optUpgrade opts
                             , allowDowngrades = optDowngrade opts
                             , allowReinstall  = optReinstall opts
                             , importMode = optImportMode opts }
                                               
verifyApp opts path = do
  inform "verifying appliance..."
  withConvertedAppliance opts path $ \_ _ _ -> inform "OK"

cleanupInv = do
  mapM_ cleanup =<< enumAppliances
  liftIO $ rmDirRec "/storage/_apptemp_"
  where
    cleanup id@(AppID name v)
      | "_temp_" `isPrefixOf` name = removeApp name
      | otherwise = return ()

main = do
  (opts,_,_) <- getOpt Permute options <$> getArgs
  let rpcCmd f = rpcConnect >>= \ctx -> rpc ctx f >> return ()
      tempRoot = optTempDir opts
  case () of
    _ | Help `elem` opts                  -> usage
      | ListApp `elem` opts               -> rpcCmd $ listApp
      | CleanupInv `elem` opts            -> rpcCmd $ cleanupInv
      | Just id <- optRemove opts         -> rpcCmd $ removeApp id
      | Just vm <- optGenEnvDoc opts      -> rpcCmd $ genVmEnvDoc vm >>= liftIO . putStrLn
      |   Just vm <- optGenEnvIso opts
        , Just isopath <- optIsoFile opts -> rpcCmd $ genVmEnvIso tempRoot isopath vm
      | Just p <- optVerifyPath opts      -> verifyApp opts p
      | Just p <- optImportPath opts      -> importApp opts p
      | otherwise -> usage

