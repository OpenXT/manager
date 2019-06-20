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

{-# LANGUAGE Arrows, PatternGuards #-}
module OVF.Parse
       ( runParser
       ) where


import OVF.Model
import OVF.ModelXCI
import Data.Maybe
import Data.Char
import Data.Ord
import Data.List
import Data.String
import Text.XML.HXT.Core
--import Tools.Text
import qualified Data.Text as T
import OVF.AllocationUnit
import Core.Types

encryptionKeySizeDefault = 512

strip :: String -> String
strip = T.unpack . T.strip . T.pack

splitOnSpace = map T.unpack . filter (not . T.null) . T.split (T.pack " ") . T.pack

namespaces :: [(String,String)]
namespaces =
  [ ("ovf", "http://schemas.dmtf.org/ovf/envelope/1")
  , ("vssd", "http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/CIM_VirtualSystemSettingData")
  , ("rasd", "http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/CIM_ResourceAllocationSettingData")
  , ("sasd", "http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/CIM_StorageAllocationSettingData")
  , ("epasd", "http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/CIM_EthernetPortAllocationSettingData")
  , ("xci", "http://www.citrix.com/xenclient/ovf/1")
  ]


xmlParseOpts
  = [ withValidate no
    , withCheckNamespaces no
    , withSubstDTDEntities no
    ]

maybeA :: ArrowIf a => a b c -> a b (Maybe c)
maybeA f = (f >>> arr Just) `orElse` arr (const Nothing)

withDefault' = flip withDefault

readA :: (ArrowIf a, Read c) => a String c
readA = f $< this where
  f str = case reads str of
    [] -> none
    ((v,_):_) -> constA v
  
readOrFatalA :: (Read a) => String -> IOSArrow String a
readOrFatalA msg = readA `orElse` (issueFatal msg >>> none)

envelopeA :: IOSArrow XmlTree Envelope
envelopeA = proc x -> do
  references_ <- withDefault' [] (getChildren /> (hasName "ovf:References") >>> listA fileReferenceA) -< x
  diskSections_ <- listA diskSectionA -< x
  networkSection_ <- maybeA networkSectionA -< x
  eulas_ <- listA eulaA -< x
  content_ <- getChildren >>> contentA -< x
  returnA -<
    Envelope { references = references_, diskSections = diskSections_, networkSection = networkSection_
             , eulas = eulas_, content = content_
             }

eulaA :: IOSArrow XmlTree String
eulaA = deep (hasName "ovf:EulaSection" /> hasName "ovf:License" /> getText)

contentA :: IOSArrow XmlTree Content
contentA = collections <+> systems where
  systems = getChildren >>> virtualSystemA >>> arr ContentVirtualSystem
  collections = getChildren >>> hasName "ovf:VirtualSystemCollection" >>> proc x -> do
    id <- getAttrValue "ovf:id" -< x
    info <- infoA -< x
    name <- (getChildren >>> hasName "ovf:Name" /> getText) `orElse` (constA "") -< x
    productSections_ <- listA productSectionA -< x
    items_ <- listA contentA -< x
    returnA -< ContentVirtualSystemCollection { collectionID = id, collectionInfo = info, collectionName = name,
      collectionProductSections = productSections_, collectionItems = items_ }

fileReferenceA :: IOSArrow XmlTree FileRef
fileReferenceA = deep (hasName "ovf:File") >>> proc x -> do
  id <- getAttrValue "ovf:id" -< x
  href <- getAttrValue "ovf:href" -< x
  size <- withDefault' 0 (getAttrValue0 "ovf:size" >>> readOrFatalA "bad ovf:size attribute") -< x
  returnA -< FileRef (FileID id) href size

infoA :: IOSArrow XmlTree String
infoA = withDefault' "" (getChildren >>> hasName "ovf:Info" /> getText)

diskSectionA :: IOSArrow XmlTree DiskSection
diskSectionA = 
  (deep (hasName "ovf:DiskSection") >>> contentsA False)
  <+>
  (deep (hasName "ovf:SharedDiskSection") >>> contentsA True)
  where                
    contentsA shared = proc x -> do
      info <- infoA -< x
      disks <- listA (diskA shared) -< x
      returnA -< DiskSection info disks

diskA :: Bool -> IOSArrow XmlTree Disk
diskA shared = deep (hasName "ovf:Disk") >>> proc x -> do
  id <- getAttrValue "ovf:diskId" -< x
  fileRef <- maybeA (getAttrValue0 "ovf:fileRef") -< x
  capacity <- getAttrValue "ovf:capacity" >>> readOrFatalA "bad ovf:capacity attribute" -< x
  capacityUnits <- withDefault' auByte (getAttrValue0 "ovf:capacityAllocationUnits" >>> allocationUnitA) -< x
  popsz <- (getAttrValue0 "ovf:populatedSize" >>> readOrFatalA "bad ovf:populatedSize attribute" >>> arr Just) `orElse` constA Nothing  -< x
  f <- getAttrValue "ovf:format" -< x
  returnA -< Disk { diskID = DiskID id, diskFileRef = FileID `fmap` fileRef,
    diskCapacity = capacity, diskCapacityAllocationUnits = capacityUnits,
    diskPopulatedSize = popsz, diskShared = shared, diskFormat = f }

networkSectionA :: IOSArrow XmlTree NetworkSection
networkSectionA = deep (hasName "ovf:NetworkSection") >>> proc x -> do
  info <- infoA -< x
  networks_ <- listA networkA -< x
  returnA -< NetworkSection info networks_

networkA :: IOSArrow XmlTree Network
networkA = deep (hasName "ovf:Network") >>> proc x -> do
  name <- getAttrValue "ovf:name" -< x
  descr <- deep (hasName "ovf:Description") /> getText -< x
  returnA -< Network name descr

virtualSystemA :: IOSArrow XmlTree VirtualSystem
virtualSystemA = hasName "ovf:VirtualSystem" >>> proc x -> do
  id <- getAttrValue "ovf:id" -< x
  info <- infoA -< x
  name <- (getChildren >>> hasName "ovf:Name" /> getText) `orElse` (constA "") -< x
  envfiles <- envFilesSectionA -< x
  productSections <- listA productSectionA -< x
  (install, installDelay) <- installSectionA -< x
  items <- listA itemA >>> arr sortItems -< x
  let (resourceItems, storageItems, ethernetPortItems) = partitionItems items
  transport <- withDefault' [] (getAttrValue0 "ovf:transport" >>> arr splitOnSpace) -< x
  returnA -< VirtualSystem {
      systemID = SystemID id, systemInfo = info, systemName = name, systemProductSections = productSections
    , systemOSSection = []
    , systemEnvFiles = envfiles
    , systemResourceItems = resourceItems
    , systemEthernetPortItems = ethernetPortItems
    , systemStorageItems = storageItems
    , systemInstall = install
    , systemInstallDelay = installDelay
    , systemTransport = transport
  }

-- to ease later processing we convert ResouriceItems into StorageItems and EthernetPortItems if deemed ok
partitionItems :: [Item] -> ( [ResourceItem], [StorageItem], [EthernetPortItem] )
partitionItems = foldr part ( [], [], [] ) where
  rev (a,b,c) = (reverse a, reverse b, reverse c)
  part (SRI i) (rs, ss, eps) = (rs, i:ss, eps)
  part (EPI i) (rs, ss, eps) = (rs, ss  , i:eps)
  part (RI  i) (rs, ss, eps)
    | isStorage      = (rs  , mkStorage i : ss, eps)
    | isEthernetPort = (rs  , ss              , mkEthernetPort i : eps)
    | otherwise      = (i:rs, ss              , eps)
    where
      t = getResourceType (resTypeID i)
      isStorage | Just x <- t = x `elem` [RT_CDDrive, RT_DVDDrive, RT_HardDisk]
                | otherwise = False
      isEthernetPort | Just x <- t = x `elem` [RT_EthernetAdapter]
                     | otherwise = False

      mkStorage res = StorageItem {
          srResourceItem = res
        , srAccess = 0
        , srHostExtentName = ""
        , srHostExtentNameFormat = 0
        , srHostExtentNameNamespace = 0
        , srHostExtentStartingAddress = 0
        , srHostResourceBlockSize = 0
        , srLimit = 0
        , srOtherHostExtentNameFormat = ""
        , srOtherHostExtentNameNamespace = ""
        , srVirtualResourceBlockSize = 0
        }

      mkEthernetPort res = EthernetPortItem {
          ethResourceItem = res
        , ethDefaultPortVID = Nothing
        , ethDefaultPriority = Nothing
        , ethDesiredVLANEndpointMode = Nothing
        , ethGroupID = Nothing
        , ethManagerID = Nothing
        , ethNetworkPortProfileID = Nothing
        , ethOtherEndpointMode = Nothing
        , ethOtherNetworkPortProfileIDTypeInfo = Nothing
        , ethPortCorrelationID = Nothing
        , ethPortVID = Nothing
        , ethPromiscuous = False
        , ethReceiveBandwidthLimit = 0
        , ethReceiveBandwidthReservation = 0
        , ethSourceMACFilteringEnabled = False
        , ethAllowedPriorities = []
        , ethAllowedToReceiveMACAddresses = []
        , ethAllowedToReceiveVLANs = []
        , ethAllowedToTransmitMACAddresses = []
        , ethAllowedToTransmitVLANs = []
        }
        
productSectionA :: IOSArrow XmlTree ProductSection
productSectionA = deep (hasName "ovf:ProductSection") >>> proc x -> do
  class_ <- maybeA (getAttrValue0 "ovf:class") -< x
  instance_ <- maybeA (getAttrValue0 "ovf:instance") -< x
  info <- infoA -< x
  name <- withDefault' "" (getChildren >>> hasName "ovf:Name" /> getText) -< x
  version <- withDefault' "" (getChildren >>> hasName "ovf:Version" /> getText) -< x
  properties <- listA productPropertyA -< x
  returnA -< ProductSection {
    productClass = class_, productInstance = instance_, productInfo = info, productName = name, productVersion = version, productProperties = properties }

productPropertyA :: IOSArrow XmlTree ProductProperty
productPropertyA = deep (hasName "ovf:Property") >>> proc x -> do
  descr <- withDefault' "" (getChildren >>> hasName "ovf:Description" /> getText) -< x
  key <- getAttrValue "ovf:key" -< x
  typeStr <- getAttrValue "ovf:type" -< x
  value <- getAttrValue "ovf:value" -< x
  userConfigurableStr <- withDefault' "false" (getAttrValue0 "ovf:userConfigurable") -< x
  passwordStr <- withDefault' "false" (getAttrValue0 "ovf:password") -< x
  let userConfigurable = boolStr userConfigurableStr
      password = boolStr passwordStr
      typ = ovfTypeFromStr typeStr
  returnA -< ProductProperty {
     propertyKey = key, propertyType = typ, propertyValue = value, propertyUserConfigurable = userConfigurable
   , propertyDescription = descr, propertyPassword = password }

envFilesSectionA :: IOSArrow XmlTree [(FileID, FilePath)]
envFilesSectionA = withDefault' [] $ getChildren >>> hasName "ovf:EnvironmentFilesSection" >>> listA envFile where
  envFile = getChildren >>> hasName "ovf:File" >>> proc x -> do
    ref  <- getAttrValue0 "ovf:fileRef" -< x
    path <- getAttrValue0 "ovf:path" -< x
    returnA -< (FileID ref,path)
  
installSectionA :: IOSArrow XmlTree (Bool, Int)
installSectionA = (getChildren >>> sectionData) `orElse` constA (False, 0) where
  sectionData = hasName "ovf:InstallSection" >>> proc x -> do
    delay <- withDefault' 0 (getAttrValue0 "ovf:initialBootStopDelay" >>> readOrFatalA "bad ovf:initialBootStopDelay") -< x
    returnA -< (True, delay)

itemA :: IOSArrow XmlTree Item
itemA = (resourceItemA >>> arr RI) <+> (storageItemA >>> arr SRI) <+> (ethernetPortItemA >>> arr EPI)

resourceItemA = deep (hasName "ovf:Item") >>> resourceItemBodyA "rasd"

resourceItemBodyA :: String -> IOSArrow XmlTree ResourceItem
resourceItemBodyA nsPrefix = proc x -> do
  bound <- withDefault' "normal" (getAttrValue0 "ovf:bound") -< x
  --FIXME: currently skipping all but 'normal' bounds, implement support for bounds later
  none `whenP` (/= "normal") -< bound
  address <- withDefault' "" (subitemA "Address") -< x
  addressOnParent <- withDefault' "" (subitemA "AddressOnParent") -< x
  unit <- withDefault' auByte (subitemA "AllocationUnits" >>> allocationUnitA) -< x
  autoalloc <- maybeA (subitemA "AutomaticAllocation") >>> arr (maybe True boolStr) -< x
  descr <- withDefault' "" (subitemA "Description") -< x
  connection <- maybeA (subitemA "Connection") -< x
  hostresource <- maybeA (subitemA "HostResource") -< x
  name <- withDefault' "" (subitemA "ElementName") -< x
  instID <- readSubitemA "InstanceID" >>> arr ResInstanceID -< x
  parent <- maybeA (readSubitemA "Parent" >>> arr ResInstanceID) -< x
  typeID <- readSubitemA "ResourceType" -< x
  subtype <- withDefault' "" (subitemA "ResourceSubType") -< x
  quantity <- withDefault' 0 (readSubitemA "VirtualQuantity") -< x
  quantityUnits <- withDefault' auByte (subitemA "VirtualQuantityUnits" >>> allocationUnitA) -< x
  reservation <- withDefault' 0 (readSubitemA "Reservation") -< x
  returnA -< ResourceItem {
            resAddress = address
          , resAddressOnParent = addressOnParent
          , resAllocationUnits = unit
          , resAutomaticAllocation = autoalloc
          , resDescription = descr
          , resConnection = connection
          , resHostResource = hostresource
          , resName = name
          , resInstanceID = instID
          , resParent = parent
          , resTypeID = typeID
          , resSubType = subtype
          , resVirtualQuantity = quantity
          , resVirtualQuantityUnits = quantityUnits
          , resReservation = reservation
          }
  where
   subitemA n = getChildren >>> hasName (nsPrefix ++ ":" ++ n) /> getText
   readSubitemA n = subitemA n >>> readOrFatalA ("Item: bad " ++ n)

ethernetPortItemA :: IOSArrow XmlTree EthernetPortItem
ethernetPortItemA = deep (hasName "ovf:EthernetPortItem") >>> proc x -> do
  res <- resourceItemBodyA "epasd" -< x
  defaultPortVID <- maybeA (readSubitemA "DefaultPortVID") -< x
  defaultPriority <- maybeA (readSubitemA "DefaultPriority") -< x
  desiredVLANEndpointMode <- maybeA (readSubitemA "DesiredVLANEndpointMode") -< x
  groupID <- maybeA (readSubitemA "GroupID") -< x
  managerID <- maybeA (readSubitemA "ManagerID") -< x
  networkPortProfileID <- maybeA (subitemA "NetworkPortProfileID") -< x
  otherEndpointMode <- maybeA (subitemA "OtherEndpointMode") -< x
  otherNetworkPortProfileIDTypeInfo <- maybeA (subitemA "OtherNetworkPortProfileIDTypeInfo") -< x
  portCorrelationID <- maybeA (subitemA "PortCorrelationID") -< x
  portVID <- maybeA (readSubitemA "PortVID") -< x
  promiscuous <- withDefault' False (subitemA "Promiscuous" >>> boolA) -< x
  receiveBandwidthLimit <- withDefault' 0 (readSubitemA "ReceiveBandwithLimit") -< x
  receiveBandwidthReservation <- withDefault' 0 (readSubitemA "ReceiveBandwithReservation") -< x
  sourceMACFilteringEnabled <- withDefault' False (subitemA "SourceMACFilteringEnabled" >>> boolA) -< x
  returnA -< EthernetPortItem {
      ethResourceItem = res, ethDefaultPortVID = defaultPortVID, ethDefaultPriority = defaultPriority
    , ethDesiredVLANEndpointMode = desiredVLANEndpointMode, ethGroupID = groupID, ethManagerID = managerID
    , ethNetworkPortProfileID = networkPortProfileID, ethOtherEndpointMode = otherEndpointMode
    , ethOtherNetworkPortProfileIDTypeInfo = otherNetworkPortProfileIDTypeInfo
    , ethPortCorrelationID = portCorrelationID, ethPortVID = portVID, ethPromiscuous = promiscuous
    , ethReceiveBandwidthLimit = receiveBandwidthLimit, ethReceiveBandwidthReservation = receiveBandwidthReservation
    , ethSourceMACFilteringEnabled = sourceMACFilteringEnabled
    , ethAllowedPriorities = [], ethAllowedToReceiveMACAddresses = [], ethAllowedToReceiveVLANs = []
    , ethAllowedToTransmitMACAddresses = [], ethAllowedToTransmitVLANs = [] }
  where
    subitemA n = getChildren >>> hasName ("epasd:" ++ n) /> getText
    readSubitemA n = subitemA n >>> readOrFatalA ("EthernetPortItem: bad " ++ n)

storageItemA :: IOSArrow XmlTree StorageItem
storageItemA = deep (hasName "ovf:StorageItem") >>> proc x -> do
  res <- resourceItemBodyA "sasd" -< x
  access <- withDefault' 0 (readSubitemA "Access") -< x
  hostExtentName <- withDefault' "" (subitemA "HostExtentName") -< x
  hostExtentNameFormat <- withDefault' 0 (readSubitemA "HostExtentNameFormat") -< x
  hostExtentNameNamespace <- withDefault' 0 (readSubitemA "HostExtentNameNamespace") -< x
  hostExtentStartingAddress <- withDefault' 0 (readSubitemA "HostExtentStartingAddress") -< x
  hostResourceBlockSize <- withDefault' 0 (readSubitemA "HostResourceBlockSize") -< x
  limit <- withDefault' 0 (readSubitemA "Limit") -< x
  otherHostExtentNameFormat <- withDefault' "" (subitemA "OtherHostExtentNameFormat") -< x
  otherHostExtentNameNamespace <- withDefault' "" (subitemA "OtherHostExtentNameNamespace") -< x
  virtualResourceBlockSize <- withDefault' 0 (readSubitemA "VirtualResourceBlockSize") -< x
  returnA -< StorageItem {
      srResourceItem = res, srAccess = access, srHostExtentName = hostExtentName
    , srHostExtentNameFormat = hostExtentNameFormat, srHostExtentNameNamespace = hostExtentNameNamespace
    , srHostExtentStartingAddress = hostExtentStartingAddress, srHostResourceBlockSize = hostResourceBlockSize
    , srLimit = limit, srOtherHostExtentNameFormat = otherHostExtentNameFormat
    , srOtherHostExtentNameNamespace = otherHostExtentNameNamespace
    , srVirtualResourceBlockSize = virtualResourceBlockSize }
  where
    subitemA n = getChildren >>> hasName ("sasd:" ++ n) /> getText
    readSubitemA n = subitemA n >>> readOrFatalA ("StorageItem: bad " ++ n)

allocationUnitA :: IOSArrow String AllocationUnit
allocationUnitA = f $< this where
  f str = case allocationUnitParse str of
    Just unit -> constA unit
    Nothing   -> issueFatal ("malformed allocation units string: '" ++ show str ++ "'") >>> none
  
defaultXciApp =
  XCIAppliance { xciAppDisks = [], xciAppNetworks = [], xciAppVms = [], xciAppID = Nothing, xciAppVersion = Nothing }

xciAppA :: IOSArrow XmlTree XCIAppliance
xciAppA = xciAppA' `orElse` constA defaultXciApp

xciAppA' :: IOSArrow XmlTree XCIAppliance
xciAppA' = deep (hasName "xci:ApplianceSection") >>> proc x -> do
  appid <- maybeA (getAttrValue0 "xci:applianceId") -< x
  appv  <- maybeA (getAttrValue0 "xci:version" >>> readOrFatalA "bad appliance version") -< x
  disks <- listA (getChildren >>> xciDiskA) -< x
  networks <- listA (getChildren >>> xciNetworkA) -< x
  vms <- listA (getChildren >>> xciVmA) -< x
  returnA -< XCIAppliance { xciAppDisks = disks, xciAppNetworks = networks, xciAppVms = vms, xciAppID = appid, xciAppVersion = appv }

xciDiskA :: IOSArrow XmlTree XCIDisk
xciDiskA =  hasName "xci:Disk" >>> proc x -> do
  id <- getAttrValue0 "xci:ovfId" >>> arr DiskID -< x
  enc <- encryptionA -< x
  filesys <- maybeA (filesystemA $< getAttrValue0 "xci:filesystem") -< x
  returnA -< XCIDisk { xciDiskId = id, xciDiskEncryption = enc, xciDiskFilesystem = filesys }
  where
    filesystemA str = f (filesystemFromStr str) where
      f (Just fs) = constA fs
      f _ = issueFatal ("unknown xci:filesystem '" ++ show str ++ "'") >>> none

    encryptionA = encryptionGenerateA `orElse` encryptionImportA `orElse` constA NoEncryption
    encryptionGenerateA = getChildren >>> hasName "xci:GenerateEncryptionKey" >>>
      withDefault' 512 (getAttrValue0 "xci:keySize" >>> readOrFatalA "bad encryption key size") >>> arr GenerateCryptoKey
    encryptionImportA = getChildren >>> hasName "xci:ImportEncryptionKey" >>> getAttrValue0 "xci:fileRef" >>> arr (UseCryptoKey . FileID)

xciNetworkA :: IOSArrow XmlTree XCINetwork
xciNetworkA = hasName "xci:Network" >>> proc x -> do
  name <- getAttrValue0 "xci:name" -< x
  netid <- maybeA (getAttrValue0 "xci:clientNetworkId") -< x
  returnA -< XCINetwork { xciNetworkName = name, xciNetworkClientId = netid }

xciVmA :: IOSArrow XmlTree XCIVm
xciVmA = hasName "xci:VirtualMachine" >>> proc x -> do
  id <- getAttrValue0 "xci:ovfId" >>> arr SystemID -< x
  template <- maybeA (getAttrValue0 "xci:templateId") -< x
  uuid <- maybeA (getAttrValue0 "xci:uuid" >>> arr fromString) -< x
  netdevs <- listA (getChildren >>> xciNetworkAdapterA) -< x
  storagedevs <- listA (getChildren >>> xciStorageDeviceA) -< x
  argo <- xciArgoRulesA -< x
  rpc <- xciRpcRulesA -< x
  pci <- xciPtRulesA -< x
  dbentries <- withDefault' [] xciDBEntriesA -< x
  dsfiles <- withDefault' [] xciDomStoreFilesA -< x
  props <- xciPropertyOverridesA -< x
  returnA -< XCIVm { xciVmId = id, xciVmUuid = uuid, xciVmTemplate = template,
    xciVmPropertyOverride = props, xciVmArgoRules = argo, xciVmRpcRules = rpc, xciVmPtRules = pci,
    xciVmDB = dbentries, xciVmDomStoreFiles = dsfiles,
    xciVmNetworkAdapters = netdevs, xciVmStorageDevices = storagedevs
  }

xciNetworkAdapterA :: IOSArrow XmlTree XCINetworkAdapter
xciNetworkAdapterA = hasName "xci:NetworkAdapter" >>> proc x -> do
  id <- (getAttrValue0 "xci:ovfInstanceId"  >>> readOrFatalA "bad instance id" >>> arr ResInstanceID) -< x
  props <- xciPropertyOverridesA -< x
  returnA -< XCINetworkAdapter { xciNetworkAdapterId = id, xciNetworkAdapterPropertyOverride = props }

xciStorageDeviceA :: IOSArrow XmlTree XCIStorageDevice
xciStorageDeviceA = hasName "xci:StorageDevice" >>> proc x -> do
  id <- (getAttrValue0 "xci:ovfInstanceId" >>> readOrFatalA "bad instance id" >>> arr ResInstanceID) -< x
  props <- xciPropertyOverridesA -< x
  returnA -< XCIStorageDevice { xciStorageDeviceId = id, xciStorageDevicePropertyOverride = props }

xciPropertyOverridesA :: IOSArrow XmlTree [XCIPropertyOverride]
xciPropertyOverridesA = 
  withDefault' [] ( getChildren >>> hasName "xci:PropertyOverride" >>> listA xciPropertyOverrideA )

xciPropertyOverrideA :: IOSArrow XmlTree XCIPropertyOverride
xciPropertyOverrideA = deep (hasName "xci:Property") >>> proc x -> do
  key <- getAttrValue "xci:name" -< x
  value <- getAttrValue "xci:value" -< x
  returnA -< XCIPropertyOverride key value

xciArgoRulesA :: IOSArrow XmlTree [String]
xciArgoRulesA = withDefault' [] ( deep (hasName "xci:ArgoFirewall") >>> listA xciArgoRuleA )

xciArgoRuleA :: IOSArrow XmlTree String
xciArgoRuleA = deep (hasName "xci:ArgoRule") /> getText >>> arr strip

xciRpcRulesA :: IOSArrow XmlTree [String]
xciRpcRulesA = withDefault' [] ( deep (hasName "xci:RpcFirewall") >>> listA xciRpcRuleA )

xciRpcRuleA :: IOSArrow XmlTree String
xciRpcRuleA = deep (hasName "xci:RpcRule") /> getText >>> arr strip

xciPtRulesA :: IOSArrow XmlTree [PtRule]
xciPtRulesA = withDefault' [] ( deep (hasName "xci:PCIPassthrough") >>> listA xciPtRuleA )

xciPtRuleA :: IOSArrow XmlTree PtRule
xciPtRuleA = byID <+> byBDF where
  byID = deep (hasName "xci:MatchID") >>> proc x -> do
    cls <- maybeA (getAttrValue0 "xci:class" >>> readOrFatalA "bad pci class") -< x
    vendor <- maybeA (getAttrValue0 "xci:vendor" >>> readOrFatalA "bad pci vendor") -< x
    dev <- maybeA (getAttrValue0 "xci:device" >>> readOrFatalA "bad pci device") -< x
    returnA -< PtMatchID cls vendor dev
  byBDF = deep (hasName "xci:MatchBDF") >>> proc x -> do
    bdf <- getAttrValue "xci:bdf" -< x
    returnA -< PtMatchBDF bdf

xciSystemTemplateIDA :: IOSArrow XmlTree (Maybe String)
xciSystemTemplateIDA = maybeA (getChildren >>> hasName "xci:SystemTemplate" /> getText >>> arr strip)
  
xciDomStoreFilesA :: IOSArrow XmlTree [FileID]
xciDomStoreFilesA = listA domStoreFileA where
  domStoreFileA = deep (hasName "xci:DomStoreFile") >>> getAttrValue0 "xci:fileRef" >>> arr FileID

xciDBEntriesA :: IOSArrow XmlTree [DBEntry]
xciDBEntriesA = listA xciDBEntryA

xciDBEntryA :: IOSArrow XmlTree DBEntry
xciDBEntryA = deep (hasName "xci:DBEntry") >>> proc x -> do
  section <- withDefault' VmSection (mkDBSection $< getAttrValue0 "xci:section") -< x
  key <- getAttrValue0 "xci:key" -< x
  v <- getAttrValue0 "xci:value" -< x
  returnA -< DBEntry section key v
  where
    mkDBSection "vm" = constA VmSection
    mkDBSection "vm-domstore" = constA DomStoreSection
    mkDBSection x = issueFatal ("bad xci:section value '" ++ x ++ "'") >>> none

boolA :: IOSArrow String Bool
boolA = arr boolStr

boolStr :: String -> Bool
boolStr x = case map toLower x of
  "true" -> True
  _ -> False

ovfTypeFromStr :: String -> PPType
ovfTypeFromStr x = f (map toLower x) where
  f "uint8" = PPT_Uint8
  f "sint8" = PPT_Sint8
  f "uint16" = PPT_Uint16
  f "sint16" = PPT_Sint16
  f "uint32" = PPT_Uint32
  f "sint32" = PPT_Sint32
  f "uint64" = PPT_Uint64
  f "sint64" = PPT_Sint64
  f "string" = PPT_String
  f "boolean" = PPT_Bool
  f "real32" = PPT_Real32
  f "real64" = PPT_Real64
  f _ = PPT_String

sortItems :: [Item] -> [Item]
sortItems = sortBy (comparing instID) where
  u (ResInstanceID x) = x
  instID (RI i) = u $ resInstanceID i
  instID (SRI i) = u $ resInstanceID $ srResourceItem i
  instID (EPI i) = u $ resInstanceID $ ethResourceItem i
  
runParser :: FilePath -> IO (Maybe (Envelope, XCIAppliance))
runParser path
  = rv =<< runX ( errorMsgStderrAndCollect
                           >>> readDocument xmlParseOpts path
                           >>> propagateNamespaces
                           >>> normaliseNamespaces
                           >>> uniqueNamespacesFromDeclAndQNames
                           >>> (envelopeA &&& xciAppA) &&& getErrStatus
                         )
    where
      normaliseNamespaces = fromLA $ cleanupNamespaces (constA namespaces >>> unlistA)

      rv [] = return Nothing
      rv (((env,xci),status):_) 
        | status == 0 = return $ Just (env,xci)
        | otherwise = return $ Nothing
