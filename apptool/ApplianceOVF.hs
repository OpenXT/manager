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

--
-- Conversion of OVF model to (xc) appliance model
--

module ApplianceOVF
       (
         OVFConvertError
       , applianceFromOVF
       )
       where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
--import Data.Functor.Identity
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Text.Lazy as TL

import qualified Network.DBus.Introspect as I
import Rpc.Core

import Core.Types
import Appliance
import VirtualSystem
import qualified OVF.Parse as Ovf
import qualified OVF.Model as Ovf
import qualified OVF.ModelXCI as Ovf
import qualified OVF.AllocationUnit as Ovf
import Idl
import ParseDBus
import Vm.ArgoFirewall ( parseRule, Rule )

import qualified Data.ByteString.UTF8 as UTF8
import qualified Network.DBus as DBus

import Tools.Text

defaultMemoryMB = 512
defaultVcpus = 1
defaultNetworkID = "/wired/0/bridged"

data OVFConvertError
   = UnknownError
   | UndefinedNetworkName Ovf.EthernetPortItem String
   | UndefinedDisk Ovf.DiskID
   | UndefinedFile Ovf.FileID
   | UnknownDiskFormat String
   | MissingDiskHostResource Ovf.StorageItem
   | InvalidURI String
   | InvalidHostResourceURIScheme String
   | InvalidArgoRule String
   | InvalidHostResource String
   | PropertyConvertError PropertyConvertError
     deriving Show

type HostResurce = URI

-- FIXME: remove the instance decl from xch-rpc
instance IsRemoteError OVFConvertError
  where fromRemoteErr = undefined
        toRemoteErr = undefined
--instance Control.Monad.Error.Error OVFConvertError

data ConvertContext = ConvertContext { idlRepo :: IdlRepo }
type Convert a = ErrorT OVFConvertError (Reader ConvertContext) a

context :: Convert ConvertContext
context = ask
idl = idlRepo <$> context

type Input = (Ovf.Envelope, Ovf.XCIAppliance)

applianceFromOVF :: IdlRepo -> Input -> Either OVFConvertError App
applianceFromOVF idl e = runReader (runErrorT (applianceFromOVF' e)) context
  where
    context = ConvertContext idl

applianceFromOVF' :: Input -> Convert App
applianceFromOVF' (e,xci) =
  do let appid = AppID unique_id version
     content <- convertContent xci files disks networks (Ovf.content e)
     return App { appID = appid, appContent = content, appAssets = [] }
  where
    networks = maybe [] Ovf.networks $ Ovf.networkSection e
    disks = concatMap Ovf.disks (Ovf.diskSections e)
    files = Ovf.references e
    unique_id = fromMaybe "unnamed" $ Ovf.xciAppID xci
    version = fromMaybe 1 $ Ovf.xciAppVersion xci

convertContent :: Ovf.XCIAppliance -> [Ovf.FileRef] -> [Ovf.Disk] -> [Ovf.Network] -> Ovf.Content -> Convert Content
convertContent xci files disks networks content = conv content where
  conv (Ovf.ContentVirtualSystem s)
    = ContentVirtualSystem <$>
        convertSystem xci xcivm files disks networks s
      where xcivm = Ovf.getXciVm xci (Ovf.systemID s)
  conv c@(Ovf.ContentVirtualSystemCollection {}) = do
    productProperties_ <- convertProductProperties
             [ (section,p) | section <- Ovf.collectionProductSections c
                           , p <- Ovf.productProperties section ]
    items_ <- mapM (convertContent xci files disks networks) (Ovf.collectionItems c)
    return $ ContentVirtualSystemCollection $ VirtualSystemCollection
      { collectionID = CollectionID (Ovf.collectionID c), collectionInfo = Ovf.collectionInfo c, collectionName = Ovf.collectionName c
      , collectionProductProperties = productProperties_, collectionItems = items_ }
  
convertSystem :: Ovf.XCIAppliance -> Maybe Ovf.XCIVm -> [Ovf.FileRef] -> [Ovf.Disk] -> [Ovf.Network] -> Ovf.VirtualSystem -> Convert VirtualSystem
convertSystem xciapp xcivm files disks networks sys = do
  let overrides = fromMaybe [] (Ovf.xciVmPropertyOverride <$> xcivm)
      argo_rules = fromMaybe [] (Ovf.xciVmArgoRules <$> xcivm)
      rpc_rules = fromMaybe [] (Ovf.xciVmRpcRules <$> xcivm)
      ds_files  = fromMaybe [] (Ovf.xciVmDomStoreFiles <$> xcivm)
      db_entries= fromMaybe [] (Ovf.xciVmDB <$> xcivm)
      pt_rules  = fromMaybe [] (Ovf.xciVmPtRules <$> xcivm)

  nics_   <- convertNICs xciapp xcivm networks ethernetPortItems
  disks_  <- convertDisks xciapp xcivm files disks storageItems sysid
  pprops_ <- convertProductProperties
             [ (section,p) | section <- productSections
                           , p <- Ovf.productProperties section ]
  envFiles <- mapM (convertEnvFile files) (Ovf.systemEnvFiles sys)
  
  props_ <- convertPropertyOverrides overrides =<< ( vmIdl <$> idl )
  argorules_ <- mapM convertArgoRule argo_rules
  rpc_rules_ <- mapM convertRpcRule rpc_rules
  domStoreFiles <- mapM (convertDomStoreFile files) ds_files

  return VirtualSystem {
      vsID = sysid
    , vsUuid = join (Ovf.xciVmUuid <$> xcivm)
    , vsName = Ovf.systemName sys
    , vsTemplate = TemplateID <$> (join (Ovf.xciVmTemplate <$> xcivm))
    , vsTransportIso = "iso" `elem` Ovf.systemTransport sys
    , vsMemory = mem
    , vsVcpus = vcpus
    , vsInstallBootStopDelay = case (Ovf.systemInstall sys, Ovf.systemInstallDelay sys) of
      (True, d) -> Just d
      _ -> Nothing
    , vsPropertyOverrides = props_
    , vsProductProperties = pprops_
    , vsDisks = disks_
    , vsNICs = nics_
    , vsPciPt = pt_rules
    , vsArgoFirewall = argorules_
    , vsRpcFirewall = rpc_rules_
    , vsDB = db_entries
    , vsEnvFiles = envFiles
    , vsDomStoreFiles = domStoreFiles
  }
  where
    sysid | Ovf.SystemID ovfid <- Ovf.systemID sys = VirtualSystemID ovfid
    storageItems = Ovf.systemStorageItems sys
    ethernetPortItems = Ovf.systemEthernetPortItems sys
    rsitems = Ovf.systemResourceItems sys
    mem = fromMaybe defaultMemoryMB . Ovf.resourcedMemoryMB $ rsitems
    vcpus = fromMaybe defaultVcpus . Ovf.resourcedVcpus $ rsitems
    productSections = Ovf.systemProductSections sys

convertProductProperties :: [(Ovf.ProductSection, Ovf.ProductProperty)] -> Convert [ProductProperty]
convertProductProperties = mapM convertProductProperty

convertProductProperty (section, p) = return $
    ProductProperty { ppClass = fromMaybe "" (Ovf.productClass section)
                    , ppInstance = fromMaybe "" (Ovf.productInstance section)
                    , ppKey = Ovf.propertyKey p
                    , ppValue = Ovf.propertyValue p
                    , ppPassword = Ovf.propertyPassword p
                    , ppUserConfigurable = Ovf.propertyUserConfigurable p
                    , ppDescription = Ovf.propertyDescription p
                    , ppType = convertOVFProductPropertyType (Ovf.propertyType p)
                    }

convertOVFProductPropertyType = f where
  f Ovf.PPT_Uint8 = PPT_Uint8
  f Ovf.PPT_Sint8 = PPT_Sint8
  f Ovf.PPT_Uint16 = PPT_Uint16
  f Ovf.PPT_Sint16 = PPT_Sint16
  f Ovf.PPT_Uint32 = PPT_Uint32
  f Ovf.PPT_Sint32 = PPT_Sint32
  f Ovf.PPT_Uint64 = PPT_Uint64
  f Ovf.PPT_Sint64 = PPT_Sint64
  f Ovf.PPT_Bool = PPT_Bool
  f Ovf.PPT_String = PPT_String
  f Ovf.PPT_Real32 = PPT_Real32
  f Ovf.PPT_Real64 = PPT_Real64

convertArgoRule r = case isJust (parseRule r) of
  False -> throwError (InvalidArgoRule r)
  True -> return r

-- FIXME: validate rpc firewall rules!
convertRpcRule r = return r

convertNICs :: Ovf.XCIAppliance -> Maybe Ovf.XCIVm -> [Ovf.Network] -> [Ovf.EthernetPortItem] -> Convert [NIC]
convertNICs xciapp xcivm networks items = mapM conv (zip [0..] items) where
    conv (index,item)
      = do path <- networkPathFrom item
           userprops <- convertPropertyOverrides adapter_overrides =<< ( nicIdl <$> idl )
           let props =
                 (if null macStr then [] else [macOverride]) `catPropertyOverrides` userprops
           -- add mac address
           return (NIC index enable path props)
      where
        xciadapter = join (Ovf.getXciNetworkAdapter <$> xcivm <*> pure (Ovf.resInstanceID res))
        xcinetwork name = Ovf.getXciNetwork xciapp name
        xcinetwork_path name = join (Ovf.xciNetworkClientId <$> xcinetwork name)
        
        adapter_overrides = fromMaybe [] (Ovf.xciNetworkAdapterPropertyOverride <$> xciadapter)
        res = Ovf.ethResourceItem item
        enable = Ovf.resAutomaticAllocation res
        macOverride = DBusProperty "com.citrix.xenclient.vmnic" "mac" (dbusStr macStr)
        macStr = replace "-" ":" (Ovf.resAddress res)
        networkPathFrom item = fromConnection (Ovf.resConnection res) where
          fromConnection Nothing = return ""
          fromConnection (Just name)
            | (network:_) <- filter (\n -> name == Ovf.networkName n) networks
              = return (fromMaybe defaultNetworkID $ xcinetwork_path name)
            | otherwise = throwError (UndefinedNetworkName item name)

data SRIHostResource
   = HR_DiskRef Ovf.DiskID
   | HR_FileRef Ovf.FileID
     deriving (Eq, Show)

getSRIHostResource :: Ovf.StorageItem -> Convert (Maybe (URI,SRIHostResource))
getSRIHostResource item = convert maybeHr where
  convert Nothing = return Nothing
  convert (Just hr) = do
    uri <- maybe (throwError $ InvalidURI hr) return (parseURI hr)
    case uriScheme uri of
          "ovf" -> byLocation uri (uriLocation uri)
          _     -> throwError $ InvalidHostResourceURIScheme (uriScheme uri)
  maybeHr = Ovf.resHostResource $ Ovf.srResourceItem $ item
  byLocation uri l
      | "/disk/" `isPrefixOf` l = return . Just $ (uri, HR_DiskRef (Ovf.DiskID (drop 6 l)))
      | "/file/" `isPrefixOf` l = return . Just $ (uri, HR_FileRef (Ovf.FileID (drop 6 l)))
      | otherwise = throwError $ InvalidHostResource (show uri)
  
convertDisks :: Ovf.XCIAppliance -> Maybe Ovf.XCIVm -> [Ovf.FileRef] -> [Ovf.Disk] -> [Ovf.StorageItem] -> VirtualSystemID -> Convert [Disk]
convertDisks xciapp xcivm fileRefs disks items sysid
  = mapM (convertDisk xciapp xcivm fileRefs disks sysid) (zip [0..] $ items)

data DiskData = DiskData Ovf.Disk (Maybe Ovf.XCIDisk)

convertDisk :: Ovf.XCIAppliance -> Maybe Ovf.XCIVm -> [Ovf.FileRef] -> [Ovf.Disk] -> VirtualSystemID -> (Int,Ovf.StorageItem) -> Convert Disk
convertDisk xciapp xcivm fileRefs disks sysid (index,item) =
  convert =<< getSRIHostResource item
  where
    convert sriHR
      | harddisk, Just (uri, HR_FileRef _      ) <- sriHR = throwError (InvalidHostResource (show uri))
      | harddisk, Just (uri, HR_DiskRef diskID ) <- sriHR = makeHardDisk diskID
      
      | cdrom   , Nothing <- sriHR                        = emptyCDROM
      | cdrom   , Just (uri, HR_DiskRef _      ) <- sriHR = throwError (InvalidHostResource (show uri))
      | cdrom   , Just (uri, HR_FileRef fileID ) <- sriHR = imagedCDROM fileID
      
      | Just (uri, _) <- sriHR = throwError (InvalidHostResource (show uri))
      | otherwise = throwError (MissingDiskHostResource item)
                    
    makeHardDisk diskID
      | (disk:_) <- disksByID disks diskID = makeHardDisk' disk
      | otherwise = throwError (UndefinedDisk diskID)
      where
        makeHardDisk' disk = diskImageTypeFromFormat (Ovf.diskFormat disk) >>= f where
          f imgtype | Just fileID <- Ovf.diskFileRef disk = imagedHardDisk fileID diskdata imgtype
                    | otherwise = emptyHardDisk diskdata imgtype
          diskdata = DiskData disk xcidisk
          xcidisk = Ovf.getXciDisk xciapp diskID
    
    diskid = DiskID sysid index
    encryption xcidisk = case Ovf.xciDiskEncryption <$> xcidisk of
      Nothing -> return NoEncryption
      Just (Ovf.NoEncryption) -> return NoEncryption
      Just (Ovf.GenerateCryptoKey sz) -> return $ GenerateCryptoKey sz
      Just (Ovf.UseCryptoKey fileid) -> case fileRefsByID fileRefs fileid of
        (fileRef:_) -> UseCryptoKey <$> fileResourceFromRef fileRef
        _ -> throwError (UndefinedFile fileid)              

    emptyHardDisk (DiskData disk xcidisk) imgtype
      = encryption xcidisk >>= \encr ->
        Disk <$> pure diskid
             <*> pure enable
             <*> pure False
             <*> pure access
             <*> pure (Just $ DiskImage diskimageid imgtype (Ovf.diskShared disk) capacityBytes encr fs Nothing)
             <*> properties
             where diskimageid = let Ovf.DiskID id = Ovf.diskID disk in DiskImageID id
                   capacityBytes = (Ovf.allocationUnitBytes (Ovf.diskCapacityAllocationUnits disk) * Ovf.diskCapacity disk)
                   fs = join (Ovf.xciDiskFilesystem <$> xcidisk)
    emptyCDROM
      = Disk <$> pure diskid
             <*> pure enable
             <*> pure True
             <*> pure access
             <*> pure Nothing
             <*> properties
    
    imagedHardDisk fileID (DiskData disk xcidisk) imgtype
      | (fileRef:_) <- fileRefsByID fileRefs fileID
        = fileResourceFromRef fileRef >>= \fileRes ->
          encryption xcidisk >>= \encr ->
          Disk <$> pure diskid
               <*> pure enable
               <*> pure False
               <*> pure access
               <*> pure (Just $ DiskImage diskimageid imgtype (Ovf.diskShared disk) capacityBytes encr fs (Just fileRes))
               <*> properties
      | otherwise = throwError (UndefinedFile fileID)
        where diskimageid = let Ovf.DiskID id = Ovf.diskID disk in DiskImageID id
              fs = join (Ovf.xciDiskFilesystem <$> xcidisk)
              capacityBytes = (Ovf.allocationUnitBytes (Ovf.diskCapacityAllocationUnits disk) * Ovf.diskCapacity disk)

    imagedCDROM fileID
      | (fileRef:_) <- fileRefsByID fileRefs fileID
        = fileResourceFromRef fileRef >>= \fileRes ->
          Disk <$> pure diskid
               <*> pure enable
               <*> pure True
               <*> pure access
               <*> pure (Just $ DiskImage (DiskImageID $ strFileResource fileRes) ISO True 0 NoEncryption Nothing (Just fileRes))
               <*> properties
      | otherwise = throwError (UndefinedFile fileID)

    xcistoragedev_override = fromMaybe [] (Ovf.xciStorageDevicePropertyOverride <$> xcistoragedev)
    xcistoragedev = join (Ovf.getXciStorageDevice <$> xcivm <*> pure (Ovf.resInstanceID res))
    
    properties = convertPropertyOverrides xcistoragedev_override =<< ( diskIdl <$> idl )

    diskImageTypeFromFormat fmt = case makeImageType fmt of
      Nothing -> throwError (UnknownDiskFormat fmt)
      Just ty -> return ty
      where
        -- FIXME: what's the valid format string for microsoft vhds?
        makeImageType "vhd" = Just VHD
        makeImageType "rawfilesystem" = Just RawFilesystem
        makeImageType "cpiobz2" = Just CPIO
        makeImageType _ = Nothing

    res = Ovf.srResourceItem item
    enable = Ovf.resAutomaticAllocation res
    Just typ = Ovf.getResourceType (Ovf.resTypeID res)
    cdrom = typ `elem` [Ovf.RT_CDDrive, Ovf.RT_DVDDrive]
    harddisk = typ `elem` [Ovf.RT_HardDisk]
    access | cdrom = DiskAccessRead
           | otherwise =
                  case Ovf.srAccess item of
                    1 -> DiskAccessRead
                    2 -> DiskAccessWrite
                    3 -> DiskAccessReadWrite
                    _ | cdrom -> DiskAccessRead
                    _ -> DiskAccessReadWrite

fileResourceFromStr :: String -> Convert FileResource
fileResourceFromStr str
  = maybe (throwError $ InvalidURI str)
          (return . FileResource)
          (parseURI str)

fileResourceFromRef :: Ovf.FileRef -> Convert FileResource
fileResourceFromRef = fileResourceFromStr . Ovf.fileHref

convertDomStoreFile :: [Ovf.FileRef] -> Ovf.FileID -> Convert DomStoreFile
convertDomStoreFile fileRefs fileID
  | (ref:_) <- fileRefsByID fileRefs fileID = fromRef ref
  | otherwise = throwError (UndefinedFile fileID)
    where
      fromRef r = DomStoreFile <$> fileResourceFromRef r <*> pure ""

convertEnvFile :: [Ovf.FileRef] -> (Ovf.FileID, FilePath) -> Convert EnvFile
convertEnvFile fileRefs (fileID, relpath)
  | (ref:_) <- fileRefsByID fileRefs fileID = fromRef ref
  | otherwise = throwError (UndefinedFile fileID)
    where
      fromRef r = EnvFile <$> fileResourceFromRef r <*> pure (sanitise relpath)
      sanitise = dropWhile (\c -> c == '/' || isSpace c)

convertPropertyOverrides :: [Ovf.XCIPropertyOverride] -> I.Object -> Convert [DBusProperty]
convertPropertyOverrides overrides idl = mapM (\o -> convertPropertyOverride o idl) overrides

convertPropertyOverride (Ovf.XCIPropertyOverride key value) idl = from $ toDBusProperty key value idl where
  from (Left err) = throwError $ PropertyConvertError err
  from (Right v) = return v

data PropertyConvertError =
     PropertyNotFound String
   | PropertyNotWritable String
   | PropertyValueParseError String String
     deriving Show


toDBusProperty :: String -> String -> I.Object -> Either PropertyConvertError DBusProperty
toDBusProperty key value idl = from $ findIDLProperty idl key where
  from xs = from' (sortBy comparePreferred xs)
  -- prefer unrestricted interfaces
  comparePreferred a b =
    case () of
      _ | unrestricted a, not (unrestricted b) -> LT
        | unrestricted b, not (unrestricted a) -> GT
        | otherwise -> EQ
     where
       unrestricted (intf,_) = ".unrestricted" `isSuffixOf` (TL.unpack $ I.intfName intf)
  from' [] = Left (PropertyNotFound key)
  from' ((intf,property):_)
    | not(writable property) = Left (PropertyNotWritable key)
    | Just v <- parseDBus value (I.propType property) = Right $ DBusProperty property_name property_intf v
    | otherwise = Left (PropertyValueParseError key value)
      where
        property_name = TL.unpack $ I.propName property
        property_intf = TL.unpack $ I.intfName intf

writable property = case I.propAccess property of
  I.Write -> True
  I.ReadWrite -> True
  _ -> False

disksByID :: [Ovf.Disk] -> Ovf.DiskID -> [Ovf.Disk]
disksByID disks id = filter (\d -> Ovf.diskID d == id) disks

fileRefsByID :: [Ovf.FileRef] -> Ovf.FileID -> [Ovf.FileRef]
fileRefsByID refs id = filter (\f -> Ovf.fileID f == id) refs

dbusStr = DBus.DBusString . DBus.PackedString . UTF8.fromString
