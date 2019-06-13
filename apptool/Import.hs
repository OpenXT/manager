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

{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, MultiParamTypeClasses, ScopedTypeVariables #-}
module Import
       (
         importAppliance
       , ImportOptions (..)
       ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Concurrent
import Data.String
import Data.Maybe
import Data.Time
import qualified Control.Exception as E
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.UTF8 as UTF8
import System.FilePath
import System.Directory
import System.IO
import qualified Network.DBus as DBus
import Text.Printf

import Tools.FreezeIOM
import Tools.Db
import Tools.Process
import Tools.IfM
import Tools.Misc

import Vm.ProductProperty
import Util

import Core.Types
import Appliance
import AppInventory
import VirtualSystem
import Rpc.Core
import Rpc.Autogen.XenmgrClient
import Rpc.Autogen.XenmgrVmClient
import Rpc.Autogen.VmNicClient
import Rpc.Autogen.VmDiskClient
import Errors

import Import.Types
import Import.Monad
import Import.Files
import Import.Images

newtype VmPath = VmPath String
newtype NicPath = NicPath String
newtype DiskPath = DiskPath String

inform :: MonadIO m => String -> m ()
inform = liftIO . hPutStrLn stderr

xenmgrObj  = "/"
xenmgrSrv  = "com.citrix.xenclient.xenmgr"

withXenmgr f = f xenmgrSrv xenmgrObj
withXenmgrVm (VmPath vm) f = f xenmgrSrv vm
withVmNic (NicPath nic) f = f xenmgrSrv nic
withVmDisk (DiskPath d) f = f xenmgrSrv d

importAppliance :: FilePath -> ImportOptions -> App -> IO ()
importAppliance ovfroot options app =
  do rpc_context <- rpcConnectTo SystemBus
     state <- newMVar (ImportState [])
     let imp_context = ImportContext ovfroot options (appID app) state
     errors =<< runImportRpc rpc_context ( runImport imp_context (doImport app) )
     where
       errors (Left x) = error (show x)
       errors _ = return ()

onlyImages :: Import Bool
onlyImages = do
  mode <- importMode <$> options
  case mode of
    ImportImagesOnly _ -> return True
    _ -> return False

unregisterAppliance' appid = whenM (not <$> onlyImages) (unregisterAppliance appid True True)

-- use temporary id, folders etc while installing, relink at last moment
installApplianceWithTemporaryStructure :: Import a -> Import () -> Import a 
installApplianceWithTemporaryStructure install finalise = go =<< app where
  go id@(AppID name version) = applianceTempFolder >>= \temp -> local context' (body temp)
    where
      context' c = c { importAppID = temp_id }
      temp_id = AppID ("_temp_" ++ name) version
      body tempFolder = do
        -- cleanup the files temp folder
        liftIO $ rmDirRec tempFolder
        -- also erase it on errors/finish
        finallyCME (liftIO (rmDirRec tempFolder)) $ do
          whenM (not <$> onlyImages) $ do
            dbRm (appDBPath temp_id)
            dbWrite (appDBPath temp_id ++ "/temp") True
            dbWrite (appDBPath temp_id ++ "/version") version
            changeApplianceState temp_id AppInstalling
          rv <- (do r <- install 
                    liftIO $ finalRelinkFiles tempFolder
                    -- custom finalisation code (i.e. remove previous appliance on upgrade etc)
                    finalise
                    return r)
                      `catchError` (\e -> unregisterAppliance' temp_id >> throwError e)
          whenM (not <$> onlyImages) $ do
            changeApplianceState temp_id AppInstalled
            -- relink db
            dbMv (appDBPath temp_id) (appDBPath id)
            dbRm (appDBPath id ++ "/temp")
          return rv
  
finalRelinkFiles :: FilePath -> IO ()
finalRelinkFiles tempFolder = do
  -- final relink of files from temp folder into destination folders
  inform "moving files.."
  forEveryDirFileRec tempFolder (relinkTest tempFolder)
  forEveryDirFileRec tempFolder (relinkFile tempFolder)
                              
relinkTest :: FilePath -> FilePath -> IO ()
relinkTest tempFolder f = do
  let dst = drop (length tempFolder) f
  exist <- doesFileExist dst
  when exist $ error ("file " ++ dst ++ " already exists and would be overwritten, aborting!")

relinkFile :: FilePath -> FilePath -> IO ()
relinkFile tempFolder f = do
  let dst = drop (length tempFolder) f
  inform $ "move " ++ f ++ " -> " ++ dst
  createDirectoryIfMissing True (takeDirectory dst)
  -- rename doesn't work cross device (i.e. /storage to /config), so have to use mv
  mvFile f dst

doImport :: App -> Import ()
doImport applianceData = removeFilesOnError $
  do preImportVerification applianceData
     appid@(AppID name _) <- app
     whenM (not <$> onlyImages) (sanitiseUpgrades =<< findAppliance name)
     installApplianceWithTemporaryStructure goImport finalise
     where
       goImport = goImportWithMode =<< (importMode <$> options)
       goImportWithMode ImportAll = importDiskImages applianceData >>= importApp
       goImportWithMode (ImportImagesOnly file) = liftIO . writeDIM file =<< importDiskImages applianceData
       goImportWithMode (ImportWithExistingImages file) = importApp =<< liftIO (readDIM file)
       importApp images = do
         let systems = contentVirtualSystems $ appContent applianceData
         vmpaths <- mapM (importVirtualSystem images) systems
         mapM_ (\(p, sys) -> installVirtualSystem p sys) (zip vmpaths systems)
       removeFilesOnError f = f `catchError` (\e -> removeFiles >> throwError e)
       removeFiles = mapM_ remove =<< addedFiles where
         remove f = liftIO $ whenM (doesFileExist f) (removeFile f)
           
       finalise = do
         -- remove original appliance if it's there
         unregisterAppliance' (appID applianceData)
       
       sanitiseUpgrades Nothing = return ()
       sanitiseUpgrades (Just (AppID appname currentVer)) = do
         AppID _ newVer <- app
         opts <- options
         case () of
           _ | newVer >  currentVer, not (allowUpgrades opts)   -> error $ msg "lower version" "--upgrade"
             | newVer <  currentVer, not (allowDowngrades opts) -> error $ msg "higher version" "--downgrade"
             | newVer == currentVer, not (allowReinstall opts)  -> error $ msg "same version" "--reinstall"
             | otherwise -> return ()
         where
           msg ver opt = printf "Appliance '%s' already exists with %s. Please specify %s to override" appname ver opt

toVmPath :: ObjectPath -> VmPath
toVmPath p = VmPath (T.unpack $ strObjectPath p)

preImportVerification :: App -> Import ()
preImportVerification app = do
  whenM (not <$> onlyImages) $ do
    vms <- map toVmPath <$> withXenmgr comCitrixXenclientXenmgrListVms
    uuids <- mapM (\vm -> fromString <$> withXenmgrVm vm comCitrixXenclientXenmgrVmGetUuid) vms
    mapM_ (verifyUuid uuids) systems
  where
    verifyUuid existing sys
      | Just uuid <- vsUuid sys, uuid `elem` existing = throwError (VmAlreadyExists uuid)
      | otherwise = return ()
    systems = contentVirtualSystems $ appContent app

importVirtualSystem :: DIM -> VirtualSystem -> Import VmPath
importVirtualSystem images sys =
  do vm <- mkVmPath <$> 
           (case vsUuid sys of
                 Nothing   -> withXenmgr comCitrixXenclientXenmgrCreateVmWithTemplate template_id
                 Just uuid -> withXenmgr comCitrixXenclientXenmgrCreateVmWithTemplateAndUuid template_id (uuidStr uuid))
     uuid <- fromString <$> withXenmgrVm vm comCitrixXenclientXenmgrVmGetUuid
     join (registerApplianceOwnedVm <$> app <*> pure uuid <*> pure (vsID sys))
     withXenmgrVm vm comCitrixXenclientXenmgrVmSetName (vsName sys)
     withXenmgrVm vm comCitrixXenclientXenmgrVmSetMemory (fromIntegral $ vsMemory sys)
     withXenmgrVm vm comCitrixXenclientXenmgrVmSetVcpus (fromIntegral $ vsVcpus sys)
     withXenmgrVm vm comCitrixXenclientXenmgrVmSetOvfTransportIso (vsTransportIso sys)
     mapM_ (importDBEntry vm uuid) $ vsDB sys
     mapM_ (importDomStoreFile uuid) $ vsDomStoreFiles sys
     mapM_ (importEnvFile uuid) $ vsEnvFiles sys
     mapM (withXenmgrVm vm comCitrixXenclientXenmgrVmAddArgoFirewallRule) (vsArgoFirewall sys)
     -- FIXME: change to use xenmgr api when its there
     when (not $ null $ vsRpcFirewall sys) $
       dbWrite ("/vm/" ++ show uuid ++ "/rpc-firewall-rules") (vsRpcFirewall sys)
     mapM_ (importPtRule vm) $ vsPciPt sys
     mapM_ (importProductProperty vm) $ vsProductProperties sys
     withXenmgrVm vm setProperties (vsPropertyOverrides sys)
     -- import disks/nics after property set (since behavior can be different depending on props)
     mapM_ (importNIC vm) $ vsNICs sys
     mapM_ (importDiskDefinition images vm) $ vsDisks sys
     return vm
     
  where
    VirtualSystemID vs_id = vsID sys
    TemplateID template_id = fromMaybe default_template_id (vsTemplate sys)
    default_template_id = TemplateID "new-vm-empty"
    
installVirtualSystem :: VmPath -> VirtualSystem -> Import ()
installVirtualSystem vm sys
  | Just d <- vsInstallBootStopDelay sys = install d
  | otherwise = return ()
  where
    install :: Int -> Import ()
    install timeout = do
      t0 <- liftIO getCurrentTime
      let t1 = addUTCTime (realToFrac timeout) t0
      runInstall t1 timeout
      --withTimeout timeout (throwError $ VmInstallTimeout (show $ vsID sys)) runInstall
    runInstall tmax timeout = do
      inform $ "starting VM " ++ (show $ vsID sys) ++ " to complete its install process..."
      withXenmgrVm vm comCitrixXenclientXenmgrVmStart
      waitShutdown tmax timeout

    expired timeout = do
      inform $ "vm installation timeout expired, stopping it & failing appliance install"
      withXenmgrVm vm comCitrixXenclientXenmgrVmDestroy
      throwError $ VmInstallTimeout (show $ vsID sys) timeout
       
    waitShutdown tmax timeout = do
      t <- liftIO getCurrentTime
      when (t > tmax) $ expired timeout
      state <- withXenmgrVm vm comCitrixXenclientXenmgrVmGetState
      if state == eVM_STATE_STOPPED
         then return ()
         else liftIO (threadDelay (5 * 10^6) >> hPutChar stderr '.') >> waitShutdown tmax timeout
    
importProductProperty :: VmPath -> ProductProperty -> Import ()
importProductProperty vm pp = do
  uuid <- fromString <$> withXenmgrVm vm comCitrixXenclientXenmgrVmGetUuid
  vmPPUpdate uuid pp
  
importDBEntry :: VmPath -> Uuid -> DBEntry -> Import ()
importDBEntry vm uuid e = do
  let key = prefix ++ "/" ++ sanitise (dbePath e)
      prefix = case dbeSection e of
        VmSection -> "/vm/" ++ show uuid
        DomStoreSection -> "/dom-store/" ++ show uuid
  dbWrite key (dbeValue e)
  where    
    -- make sure its relative
    sanitise = dropWhile (== '/')
    
importNIC :: VmPath -> NIC -> Import ()
importNIC vm nic =
  do nicP <- mkNicPath <$> withXenmgrVm vm comCitrixXenclientXenmgrVmAddNic
     when (not . null $ nicNetwork nic) $
       withVmNic nicP comCitrixXenclientVmnicSetNetwork (nicNetwork nic)
     withVmNic nicP comCitrixXenclientVmnicSetEnabled (nicEnabled nic)
     withVmNic nicP setProperties (nicPropertyOverrides nic)

importPtRule :: VmPath -> PtRule -> Import ()
importPtRule vm (PtMatchID cls vendor dev) =
  let s Nothing  = "any"
      s (Just x) = show x in
  withXenmgrVm vm comCitrixXenclientXenmgrVmPciAddPtRule (s cls) (s vendor) (s dev)
importPtRule vm (PtMatchBDF bdf) =  
  withXenmgrVm vm comCitrixXenclientXenmgrVmPciAddPtRuleBdf bdf

appSystems = contentVirtualSystems . appContent

registerDiskCryptoKeys :: DIM -> Disk -> Import ()
registerDiskCryptoKeys dim disk = case keyFile of
  Nothing  -> return ()
  Just key -> app >>= \appid -> registerApplianceOwnedFile appid key -- to clean the key up on appliance uninstall
  where
    keyFile = getDiskImportedKeyPath disk dim
    
importDiskDefinition :: DIM -> VmPath -> Disk -> Import ()
importDiskDefinition dim vm disk = registerDiskCryptoKeys dim disk >> setupDiskFromFile hostFile
  where
    hostFile = fromMaybe "" (getDiskImportedPhysPath disk dim)
         
    setupDiskFromFile :: FilePath -> Import ()
    setupDiskFromFile p = do
      diskP <- mkDiskPath <$> withXenmgrVm vm comCitrixXenclientXenmgrVmAddDisk
      let DiskPath diskPathStr = diskP
      appid <- app
      registerApplianceVmDisk appid disk diskPathStr
      withVmDisk diskP comCitrixXenclientVmdiskSetMode accessMode
      if (not $ diskIsCdrom disk)
         then do withVmDisk diskP comCitrixXenclientVmdiskAttachVhd p
         else do withVmDisk diskP comCitrixXenclientVmdiskSetPhysType "file"
                 withVmDisk diskP comCitrixXenclientVmdiskSetDevtype "cdrom"
                 withVmDisk diskP comCitrixXenclientVmdiskSetPhysPath p
                 withVmDisk diskP comCitrixXenclientVmdiskSetShared True
      withVmDisk diskP comCitrixXenclientVmdiskSetEnabled (diskEnabled disk)
      withVmDisk diskP setProperties (diskPropertyOverrides disk)
      where
        accessMode = case diskAccess disk of
          DiskAccessRead -> "r"
          DiskAccessWrite -> "w"
          DiskAccessReadWrite -> "w"
          
setProperties :: String -> String -> [DBusProperty] -> Import ()
setProperties srv path = mapM_ (setProperty srv path)

setProperty :: String -> String -> DBusProperty -> Import ()
setProperty srv path p = rpcCallOnce call >> return () where
  call = RpcCall { callDest = fromString srv
                 , callPath = fromString path
                 , callInterfaceT = fromString "org.freedesktop.DBus.Properties"
                 , callMemberT = fromString "Set"
                 , callArgs = [ str (dbusPropertyInterface p)
                              , str (dbusPropertyName p)
                              , DBus.DBusVariant (dbusPropertyValue p) ]
                 }
  str = DBus.DBusString . DBus.PackedString . UTF8.fromString

mkVmPath  = VmPath . pathStr
mkNicPath = NicPath . pathStr
mkDiskPath = DiskPath . pathStr
pathStr   = T.unpack . strObjectPath
