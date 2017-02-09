--
-- Copyright (c) 2014 Citrix Systems, Inc.
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

{-# LANGUAGE TypeSynonymInstances,OverlappingInstances,TypeOperators,PatternGuards,ScopedTypeVariables,FlexibleInstances #-}
module Vm.Config (
                  ConfigProperty

                , diagnose
                , amtPtActive

                  -- read / write / check for existence of config properties
                , readConfigProperty
                , readConfigPropertyDef
                , saveConfigProperty
                , saveOrRmConfigProperty
                , haveConfigProperty
                , locateConfigProperty
                , getConfigPropertyName

                  -- Xenvm config out of database config
                , getXlConfig
                , stringifyXlConfig

                  -- list of interesting config properties
                , vmUuidP, vmName, vmDescription, vmType, vmSlot, vmImagePath, vmPvAddons, vmPvAddonsVersion
                , vmStartOnBoot, vmStartOnBootPriority, vmKeepAlive, vmProvidesNetworkBackend, vmTimeOffset
                , vmAmtPt, vmCryptoUser, vmCryptoKeyDirs, vmStartup
                , vmNotify, vmHvm, vmPae, vmAcpi, vmApic, vmViridian, vmNx, vmSound, vmMemory, vmHap, vmSmbios
                , vmDisplay, vmBoot, vmCmdLine, vmKernel, vmInitrd, vmAcpiPath, vmVcpus, vmGpu
                , vmKernelExtract, vmInitrdExtract
                , vmMemoryStaticMax
                , vmMemoryMin
                , vmVideoram, vmHibernated, vmHidden, vmMeasured, vmShutdownPriority, vmProvidesGraphicsFallback
                , vmPorticaStatus, vmPassthroughMmio, vmPassthroughIo, vmHiddenInUi
                , vmFlaskLabel, vmCoresPerSocket, vmAutoS3Wake, vmSeamlessId, vmStartFromSuspendImage
                , vmExtraHvms, vmExtraXenvm, vmDisks, vmNics, vmPcis, vmDisk, vmNic, vmPci, vmQemuDmPath, vmQemuDmTimeout
                , vmTrackDependencies, vmSeamlessMouseLeft, vmSeamlessMouseRight
                , vmOs, vmControlPlatformPowerState
                , vmFirewallRules
                , vmSeamlessTraffic
                , vmOemAcpiFeatures, vmUsbEnabled, vmUsbAutoPassthrough, vmUsbControl, vmCpuid
                , vmStubdom, vmStubdomMemory, vmStubdomCmdline
                , vmUsbGrabDevices
                , vmGreedyPcibackBind
                , vmRunPostCreate, vmRunPreDelete, vmRunOnStateChange, vmRunOnAcpiStateChange
                , vmRunPreBoot
                , vmRunInsteadofStart
                , vmDomstoreReadAccess, vmDomstoreWriteAccess
                , vmShowSwitcher, vmWirelessControl, vmNativeExperience
                , vmXciCpuidSignature
                , vmS3Mode
                , vmS4Mode
                , vmVsnd, vmVkbd, vmVfb, vmV4v
                , vmRealm
                , vmSyncUuid
                , vmIcbinnPath
                , vmOvfTransportIso
                , vmReady
                , vmProvidesDefaultNetworkBackend
                , vmRestrictDisplayDepth
                , vmRestrictDisplayRes
                , vmPreserveOnReboot
                , vmBootSentinel
                , vmHpet, vmHpetDefault
                , vmTimerMode, vmTimerModeDefault
                , vmNestedHvm
                , vmSerial
                ) where

import Control.Arrow
import Control.Monad hiding (join)
import Control.Applicative
import Data.Bits
import Data.Char
import Data.String
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import Directory
import Text.Printf
import System.FilePath.Posix

import Tools.Log
import Tools.File
import Tools.Misc
import Tools.Future
import Tools.IfM

import Rpc.Core
import Vm.Types
import Vm.Policies
import XenMgr.Db
import XenMgr.Host
import XenMgr.Notify
import XenMgr.Rpc
import XenMgr.Config

import Rpc.Autogen.XenmgrConst

------------------------
-- Configuration Tree --
------------------------

type Location   = String
type Value      = String

-- UUID as well
instance Marshall Uuid where
    dbRead  x   = dbReadStr x >>= return . fromString
    dbWrite x v = dbWriteStr x (show v)

-- VM type can be marshalled
instance Marshall VmType where
    dbRead x  = dbReadStr x >>= return . fromS
              where
                fromS "svm"      = Svm
                fromS "pvm"      = Svm
                fromS tag        = ServiceVm tag

    dbWrite x v = dbWriteStr x (toS v)
              where
                toS Svm  = "svm"
                toS (ServiceVm tag) = tag

instance Marshall XbDeviceID where
  dbRead p = XbDeviceID <$> dbRead p
  dbWrite p (XbDeviceID v) = dbWrite p v

instance EnumMarshall DiskDeviceType where
    enumMarshallMap = [ (DiskDeviceTypeDisk , "disk" )
                      , (DiskDeviceTypeCdRom, "cdrom") ]

instance EnumMarshall DiskMode where
    enumMarshallMap = [ (Vm.Types.ReadOnly , "r")
                      , (Vm.Types.ReadWrite, "w") ]

instance EnumMarshall DiskType where
    enumMarshallMap = [ (DiskImage      , "file")
                      , (PhysicalDevice , "phy" )
                      , (QemuCopyOnWrite, "qcow")
                      , (ExternalVdi    , "vdi" )
                      , (Aio            , "aio" )
                      , (VirtualHardDisk, "vhd" )
                      , (Raw            , "raw" ) ]

instance EnumMarshall DiskSnapshotMode where
    enumMarshallMap = [ (SnapshotTemporary         , "temporary"           )
                      , (SnapshotTemporaryEncrypted, "temporary-encrypted" )
                      , (SnapshotCoalesce          , "coalesce"            )
                      , (SnapshotScripted          , "scripted"            )
                      , (SnapshotScriptedAuthor    , "scripted-author"     )
                      , (SnapshotScriptedNoSnapshot, "scripted-no-snapshot") ]

instance EnumMarshall ManagedDiskType where
  enumMarshallMap = [ (UnmanagedDisk, eMANAGED_DISKTYPE_NONE)
                    , (SystemDisk, eMANAGED_DISKTYPE_SYSTEM)
                    , (ApplicationDisk, eMANAGED_DISKTYPE_APPLICATION)
                    , (UserDisk, eMANAGED_DISKTYPE_USER) ]

instance EnumMarshall S3Mode where
  enumMarshallMap
    = [ (S3Pv, eS3_MODE_PV)
      , (S3Ignore, eS3_MODE_IGNORE)
      , (S3Restart, eS3_MODE_RESTART)
      , (S3Snapshot, eS3_MODE_SNAPSHOT) ]

instance EnumMarshall S4Mode where
  enumMarshallMap
    = [ (S4Pv, eS4_MODE_PV)
      , (S4Ignore, eS4_MODE_IGNORE)
      , (S4Restart, eS4_MODE_RESTART)
      , (S4Snapshot, eS4_MODE_SNAPSHOT) ]

instance Marshall DiskMode         where {dbRead = dbReadEnum; dbWrite = dbWriteEnum}
instance Marshall DiskType         where {dbRead = dbReadEnum; dbWrite = dbWriteEnum}
instance Marshall DiskSnapshotMode where {dbRead = dbReadEnum; dbWrite = dbWriteEnum}
instance Marshall DiskDeviceType   where {dbRead = dbReadEnum; dbWrite = dbWriteEnum}
instance Marshall ManagedDiskType  where {dbRead = dbReadEnum; dbWrite = dbWriteEnum}
instance Marshall S3Mode           where {dbRead = dbReadEnum; dbWrite = dbWriteEnum}
instance Marshall S4Mode           where {dbRead = dbReadEnum; dbWrite = dbWriteEnum}

instance Marshall Sha1Sum where
    dbRead = fmap (read . ("0x" ++)) . dbReadStr
    dbWrite x = dbWriteStr x . printf "%040x"

instance Marshall a => Marshall (Maybe a) where
    dbRead  = dbMaybeRead
    dbWrite = dbMaybeWrite

-- Disk definition info can be marshalled
instance Marshall Disk where
    dbRead x = do path    <- dbRead (x ++ "/path"    )
                  typ     <- dbRead (x ++ "/type"    )
                  mtyp    <- dbReadWithDefault UnmanagedDisk (x ++ "/managed-disktype" )
                  mode    <- dbRead (x ++ "/mode"    )
                  dev     <- dbRead (x ++ "/device"  )
                  devt    <- dbRead (x ++ "/devtype" )
                  snap    <- dbRead (x ++ "/snapshot")
                  sha1Sum <- dbRead (x ++ "/sha1sum" )
                  shared  <- dbReadWithDefault False (x ++ "/shared")
                  enable  <- dbReadWithDefault True (x ++ "/enable")
                  return $
                         Disk { diskPath         = path
                              , diskType         = typ
                              , diskMode         = mode
                              , diskDevice       = dev
                              , diskDeviceType   = devt
                              , diskSnapshotMode = snap
                              , diskSha1Sum      = sha1Sum
                              , diskShared       = shared
                              , diskManagedType  = mtyp
                              , diskEnabled      = enable
                              }
    dbWrite x v = do current <- dbRead x
                     dbWrite (x ++ "/path"    ) (diskPath         v)
                     dbWrite (x ++ "/type"    ) (diskType         v)
                     let mmt UnmanagedDisk = Nothing; mmt x = Just x
                     dbMaybeWrite (x ++ "/managed-disktype") (mmt $ diskManagedType v)
                     dbWrite (x ++ "/mode"    ) (diskMode         v)
                     dbWrite (x ++ "/device"  ) (diskDevice       v)
                     dbWrite (x ++ "/devtype" ) (diskDeviceType   v)
                     dbWrite (x ++ "/snapshot") (diskSnapshotMode v)
                     dbWrite (x ++ "/sha1sum" ) (diskSha1Sum      v)
                     dbWrite (x ++ "/shared" ) (diskShared v)
                     when (diskEnabled v /= diskEnabled current) $ 
                       dbWrite (x ++ "/enable") (diskEnabled v)

-- NIC definition can be marshalled
instance Marshall NicDef where
    dbRead x = do ids    <- dbReadStr         (x ++ "/id"          )
                  net    <- dbMaybeRead       (x ++ "/network"      )
                  uuid   <- dbMaybeRead       (x ++ "/backend-uuid")
                  bname  <- dbMaybeRead       (x ++ "/backend-name")
                  enable <- dbReadWithDefault True (x ++ "/enable"      )
                  wifi   <- dbReadWithDefault False (x ++ "/wireless-driver")
                  mac    <- dbMaybeRead       (x ++ "/mac"         )
                  let nicid = case ids of
                                "" -> 0
                                s  -> read s :: Int
                  return $ NicDef
                      { nicdefId           = XbDeviceID nicid
                      , nicdefNetwork      = fromMaybe fallbackNetwork (fmap networkFromStr net)
                      , nicdefWirelessDriver = wifi
                      , nicdefBackendUuid  = case uuid of
                        Just "" -> Nothing
                        _       -> fmap fromString uuid
                      , nicdefBackendName  = case bname of
                        Just "" -> Nothing
                        _ -> fmap id bname
                      , nicdefBackendDomid = Nothing
                      , nicdefEnable       = enable
                      , nicdefMac          = mac }

    dbWrite x v = do current <- dbRead x
                     let XbDeviceID nid = nicdefId v
                     dbWriteStr (x ++ "/id") (show nid)
                     when (nicdefEnable v /= nicdefEnable current) $
                          dbWrite    (x ++ "/enable") (nicdefEnable v)
                     dbWrite (x ++ "/network") (networkToStr $ nicdefNetwork v)
                     case nicdefWirelessDriver v of
                       False ->   dbRm    (x ++ "/wireless-driver")
                       True  ->   dbWrite (x ++ "/wireless-driver") True
                     case nicdefBackendUuid v of
                       Nothing -> dbRm    (x ++ "/backend-uuid")
                       Just id -> dbWrite (x ++ "/backend-uuid") id
                     case nicdefBackendName v of
                       Nothing -> dbRm    (x ++ "/backend-name")
                       Just id -> dbWrite (x ++ "/backend-name") id
                     case nicdefMac v of
                       Nothing -> dbRm    (x ++ "/mac")
                       Just m  -> dbWrite (x ++ "/mac") m

-- Portica status is marshallable
instance Marshall PorticaStatus where
    -- But this is dangerous, if the order of elements in PorticaStatus changes.
    dbRead x = PorticaStatus <$> (maybe False id <$> dbMaybeRead (x ++ "/portica-installed"))
                             <*> (maybe False id <$> dbMaybeRead (x ++ "/portica-enabled"))

    dbWrite x (PorticaStatus installed enabled) =
        do dbWrite (x ++ "/portica-installed") installed
           dbWrite (x ++ "/portica-enabled")   enabled


-- A path to database from given VM
dbPath :: Uuid -> Location
dbPath uuid = "/vm/" ++ show uuid

-- Convert a property path by getting rid of dots
convert :: Location -> Location
convert = map f
    where f '.' = '/'
          f x   = x

-- Join paths using a separator /
join :: [String] -> String
join = concat . intersperse "/"

data ConfigProperty = ConfigProperty { property_name :: String
                                     , property_location :: Uuid -> String }

property :: String -> ConfigProperty
property name =
    ConfigProperty
    {
      property_name = name
    , property_location = \uuid -> join [dbPath uuid, convert name]
    }

-- Locate a named property within a VM of given uuid
locate :: ConfigProperty -> Uuid -> Location
locate p uuid = property_location p uuid

locateConfigProperty = property

------------------------
-- Individual properties
------------------------

-- Core Ones
vmUuidP = property "uuid"
vmName = property "name"
vmDescription = property "description"
vmSlot = property "slot"
vmType = property "type"
vmImagePath = property "image_path"
vmPvAddons = property "pv-addons-installed"
vmPvAddonsVersion = property "pv-addons-version"
vmStartOnBoot = property "start_on_boot"
vmStartOnBootPriority = property "start_on_boot_priority"
vmStartFromSuspendImage = property "start-from-suspend-image"
vmShutdownPriority = property "shutdown-priority"
vmKeepAlive = property "keep-alive"
vmProvidesNetworkBackend = property "provides-network-backend"
vmProvidesDefaultNetworkBackend = property "provides-default-network-backend"
vmProvidesGraphicsFallback = property "provides-graphics-fallback"
vmTimeOffset = property "time-offset"
vmAmtPt = property "amt-pt"
vmHibernated = property "hibernated"
vmHidden = property "hidden"
vmHiddenInUi = property "hidden-in-ui"
vmAutoS3Wake = property "auto-s3-wake"
vmMeasured = property "measured"
vmSeamlessId = property "seamless-id"
vmTrackDependencies = property "track-dependencies"
vmSeamlessMouseLeft = property "seamless-mouse-left"
vmSeamlessMouseRight = property "seamless-mouse-right"
vmOs = property "os"
vmControlPlatformPowerState = property "control-platform-power-state"
vmSeamlessTraffic = property "seamless-traffic"
vmOemAcpiFeatures = property "oem-acpi-features"
vmUsbEnabled = property "usb-enabled"
vmUsbAutoPassthrough = property "usb-auto-passthrough"
vmUsbControl = property "usb-control"
vmUsbGrabDevices = property "usb-grab-devices"
vmStubdom = property "stubdom"
vmCpuid = property "cpuid"
vmXciCpuidSignature = property "xci-cpuid-signature"
vmGreedyPcibackBind = property "greedy-pciback-bind"

vmRunPostCreate = property "run-post-create"
vmRunPreDelete = property "run-pre-delete"
vmRunPreBoot = property "run-pre-boot"
vmRunInsteadofStart = property "run-insteadof-start"
vmRunOnStateChange = property "run-on-state-change"
vmRunOnAcpiStateChange = property "run-on-acpi-state-change"

vmDomstoreReadAccess = property "domstore-read-access"
vmDomstoreWriteAccess = property "domstore-write-access"
vmShowSwitcher = property "show-switcher"
vmWirelessControl = property "wireless-control"
vmNativeExperience = property "native-experience"

vmS3Mode = property "s3-mode"
vmS4Mode = property "s4-mode"
vmRealm = property "realm"
vmSyncUuid = property "sync-uuid"
vmIcbinnPath = property "icbinn-path"
vmOvfTransportIso = property "ovf-transport-iso"
vmReady = property "ready"
vmRestrictDisplayDepth = property "restrict-display-depth"
vmRestrictDisplayRes = property "restrict-display-res"
vmPreserveOnReboot = property "preserve-on-reboot"
vmBootSentinel = property "boot-sentinel"

-- this one is stored directly under /vm node as two entries portica-installed and portica-enabled
vmPorticaStatus = ConfigProperty { property_name = "portica-status"
                                 , property_location = dbPath}

-- Crypto Ones
vmCryptoUser = property "crypto-user"
vmCryptoKeyDirs = property "crypto-key-dirs"

-- Ones in CONFIG subtree
vmNotify = property "config.notify"
vmHvm = property "config.hvm"
vmPae = property "config.pae"
vmAcpi = property "config.acpi"
vmApic = property "config.apic"
vmViridian = property "config.viridian"
vmHap = property "config.hap"
vmNx = property "config.nx"
vmSound = property "config.sound"
vmMemory = property "config.memory"
vmMemoryStaticMax = property "config.memory-static-max"
vmMemoryMin = property "config.memory-min"
vmDisplay = property "config.display"
vmBoot = property "config.boot"
vmCmdLine = property "config.cmdline"
vmKernel = property "config.kernel"
vmKernelExtract = property "config.kernel-extract"
vmInitrd = property "config.initrd"
vmInitrdExtract = property "config.initrd-extract"
vmAcpiPath = property "config.acpi-path"
vmVcpus = property "config.vcpus"
vmSmbios = property "config.smbios"
vmVideoram = property "config.videoram"
vmPassthroughMmio = property "config.passthrough-mmio"
vmPassthroughIo = property "config.passthrough-io"
vmStartup = property "config.startup"
vmFlaskLabel = property "config.flask-label"
vmCoresPerSocket = property "config.cores-per-socket"
vmQemuDmPath = property "config.qemu-dm-path"
vmQemuDmTimeout = property "config.qemu-dm-timeout"
vmVsnd = property "config.vsnd"
vmVkbd = property "config.vkbd"
vmVfb = property "config.vfb"
vmV4v = property "config.v4v"
vmHpet = property "config.hpet"
vmHpetDefault = True
vmTimerMode = property "config.timer-mode"
vmTimerModeDefault = "no_delay_for_missed_ticks"
vmNestedHvm = property "config.nestedhvm"
vmSerial = property "config.serial"
vmStubdomMemory = property "config.stubdom-memory"
vmStubdomCmdline = property "config.stubdom-cmdline"

-- Composite ones and lists
vmExtraHvms    = property "config.extra-hvm"
vmExtraXenvm   = property "config.extra-xenvm"
vmDisks        = property "config.disk"
vmNics         = property "config.nic"
vmPcis         = property "config.pci"
vmGpu          = property "gpu"
vmExtraHvm num = property $ "config.extra-hvm." ++ show num
vmDisk num     = property $ "config.disk." ++ show num
vmNic  num     = property $ "config.nic."  ++ show num
vmPci  num     = property $ "config.pci."  ++ show num
vmFirewallRules= property "v4v-firewall-rules"

-- Read and Save a single property
-- example usage, to save a list of disks : saveP uuid vmDisks [disk1, disk2, disk3]..
--

getConfigPropertyName :: ConfigProperty -> String
getConfigPropertyName = property_name

-- Check if property exists before reading it
readConfigProperty :: (MonadRpc e m, Marshall a) => Uuid -> ConfigProperty -> m (Maybe a)
readConfigProperty uuid p = dbMaybeRead (locate p uuid)

vmExists :: (MonadRpc e m) => Uuid -> m Bool
vmExists uuid = dbExists (dbPath uuid)

saveConfigProperty :: (MonadRpc e m, Marshall a) => Uuid -> ConfigProperty -> a -> m ()
saveConfigProperty uuid p v =
    whenM (vmExists uuid) $
      dbMaybeRead (locate p uuid) >>= maybeSave
  where
    -- only save when the value is different, do nothing when it is equal. also save when it does not exist
    maybeSave Nothing                           = save
    maybeSave (Just currentV) | currentV == v   = return ()
                              | otherwise       = save
    save = do dbWrite (locate p uuid) v
              notifyVmConfigChanged uuid

saveOrRmConfigProperty :: (MonadRpc e m, Marshall a) => Uuid -> ConfigProperty -> Maybe a -> m ()
saveOrRmConfigProperty uuid p Nothing  = dbRm (locate p uuid)
saveOrRmConfigProperty uuid p (Just v) = saveConfigProperty uuid p v

haveConfigProperty :: (MonadRpc e m) => Uuid -> ConfigProperty -> m Bool
haveConfigProperty uuid p =
    dbExists (locate p uuid)

-- Read config property with default value if it doesn't exist
readConfigPropertyDef :: (MonadRpc e m, Marshall a) => Uuid -> ConfigProperty -> a -> m a
readConfigPropertyDef uuid p def =
    fromMaybe def <$> readConfigProperty uuid p

type Problem = String

-- Find problems with config if any
diagnose :: VmConfig -> [Problem]
diagnose cfg
    | vmcfgGraphics cfg == HDX, not (vmcfgPvAddons cfg) = [ "VM has HDX enabled, but PV addons are not installed" ]
    | otherwise = [ ]

------------------------------------------
-- Create a config file for running Xl
------------------------------------------

-- Xl config is a simple list of strings in the format of key=value
newtype XlConfig = XlConfig [ Param ]

type Param    = String
type UserID   = String
type VhdName  = String
type DiskSpec = Param
type NicSpec  = Param
type PciSpec  = Param

amtPtActive :: Uuid -> Rpc Bool
amtPtActive uuid = do
  -- Amt PT is active if a) system amt pt is activated b) vm amt pt is activated
  (&&) <$> haveSystemAmtPt <*> readConfigPropertyDef uuid vmAmtPt False

stringifyXlConfig :: XlConfig -> String
stringifyXlConfig (XlConfig params) = unlines params

-- Gets a xl config, given domain ID of networking domain.
getXlConfig :: VmConfig -> Rpc XlConfig
getXlConfig cfg =
    fmap (XlConfig . concat) . mapM (force <=< future) $
    [prelude, diskSpecs cfg, nicSpecs cfg, pciSpecs cfg
    , miscSpecs cfg]
  where
    uuid = vmcfgUuid cfg
    -- First section of xenvm config file
    prelude = do Just uuid <- readConfigProperty uuid vmUuidP :: Rpc (Maybe Uuid)
                 hvm <- readConfigPropertyDef uuid vmHvm False
                 name <- readConfigPropertyDef uuid vmName ""
                 let kernel = maybe [] (\path -> ["kernel='"++path++"'"]) (vmcfgKernelPath cfg)
                 let nameStr = if name == "" then [] else [("name='"++ name ++ "'")]
                 let buildType = case hvm of
                                   True  -> "hvm"
                                   False -> "generic"
                 let builder = ["builder='" ++ buildType ++ "'"]
                 let dm_args = case hvm of
                                 True  -> ["device_model_version='qemu-xen'"]
                                 False -> []

                 return $ [ "uuid='" ++ (show uuid) ++ "'"
                          , "vnc=0"
                          , "vga='stdvga'"
                          , "crypto_key_dir='" ++ (vmcfgCryptoKeyDirs cfg) ++ "'"
                          , "xci_cpuid_signature=" ++ (if vmcfgXciCpuidSignature cfg then "1" else "0")
                          , "pci_permissive=1"
                          , "pci_msitranslate=1"
                          , "pci_seize=1"
                          , "pci_power_mgmt=1"
                          ]
                            ++ nameStr
                            ++ kernel
                            ++ builder
                            ++ dm_args

-- Next section: information about disk drives
allDisks = vmcfgDisks
validDisks = filterM isDiskValid . allDisks

isDiskValid :: Disk -> Rpc Bool
isDiskValid disk =
    case diskType disk of
      VirtualHardDisk -> liftIO . doesFileExist $ diskPath disk
      _               -> return True

--build an xl config style disk list
diskSpecs :: VmConfig -> Rpc [DiskSpec]
diskSpecs cfg = do
  disklist <- dSpec
  return $ ["disk=[" ++ (concat (intersperse "," disklist)) ++ "]"]

  where
    dSpec       = mapM (diskSpec uuid) =<< disks
    disks       = filter diskEnabled <$> validDisks cfg
    uuid        = vmcfgUuid cfg

diskSpec :: Uuid -> Disk -> Rpc DiskSpec
diskSpec uuid d  = do
  stubdom <- readConfigPropertyDef uuid vmStubdom False
  return $ printf "'%s,%s,%s,%s,%s,%s'"
             (diskPath d) (fileToRaw (enumMarshall $ diskType d)) (cdType stubdom d) (diskDevice d) (enumMarshall $ diskMode d) (if ((enumMarshall $ diskDeviceType d) == "cdrom") then (enumMarshall $ diskDeviceType d) else "")
  where
    cdType stubdom d =
      case (enumMarshall $ diskDeviceType d) of
          "cdrom" -> if stubdom then "backendtype=tap" else "backendtype=phy"
          _       -> "backendtype=tap"
    fileToRaw typ = if typ == "file" then "raw" else typ

-- Next section: information about Network Interfaces
nicSpecs :: VmConfig -> Rpc [NicSpec]
nicSpecs cfg =
    do amt <- amtPtActive (vmcfgUuid cfg)
       maybeHostmac <- liftIO eth0Mac
       -- Get the configuration file entries ...
       -- ... for all the nics which are defined & enabled & pass the policy check
       nics <- filterM policyCheck . filter nicdefEnable $ vmcfgNics cfg
       let niclist = fmap (\nic -> nicSpec cfg amt maybeHostmac nic (net_domid nic)) nics
       return $ ["vif=[" ++ (concat (intersperse "," niclist)) ++ "]"]

  where
    net_domid nic = fromMaybe 0 (nicdefBackendDomid nic)
    networks  nic = filter (\n -> niHandle n == nicdefNetwork nic) (vmcfgNetworks cfg)
    isWireless nic
      = case networks nic of
          []    -> False
          (n:_) -> niIsWireless n

    policyCheck nic
        | isWireless nic
            = policyQueryWifiNetworking (vmcfgUuid cfg)
        | otherwise
            = policyQueryWiredNetworking (vmcfgUuid cfg)

nicSpec :: VmConfig -> Bool -> Maybe Mac -> NicDef -> DomainID -> String
nicSpec cfg amt eth0Mac nic networkDomID =
    let entries = bridge ++ backend ++ wireless ++ vmMac ++ nicType
    in
      "'" ++ (concat $ intersperse "," entries) ++ "'"
    where
      netinfo :: Maybe NetworkInfo
      netinfo
          = case filter (\net -> niHandle net == nicdefNetwork nic) (vmcfgNetworks cfg) of
                (ni:_) -> Just ni
                _      -> Nothing
      -- bridge name, only necessary for emulated net interfaces as qemu manages them
      bridge    = ["bridge=" ++ (TL.unpack $ strObjectPath $ networkObjectPath$ nicdefNetwork nic)]
      bridgename= niBridgeName `fmap` netinfo
      -- force backend domid for NIC if specified
      backend   = ["backend=" ++ show networkDomID]

      -- HACK: don't put device as wireless for linuxes, as we have no pv driver for that
      wireless  | nicdefWirelessDriver nic
                , vmcfgOs cfg /= Linux   = ["wireless=true"]
                | otherwise              = [ ]

      -- use mac specified in configuration as first priority
      vmMac     | Just mac <- nicdefMac nic = ["mac=" ++ mac]
      -- otherwise,
      -- If AMT is active, we set the VM mac to be equal to original eth0 mac (that is, before
      -- the bits on it got swizzled during boot)
                | Just mac <- eth0Mac, amt == True  = ["mac=" ++ unswizzleMac mac]
      -- Otherwise we do not touch the VM mac and let xenvm choose
                | otherwise                         = [ ]
      nicType   = if (vmcfgStubdom cfg) then ["type=ioemu"] else ["type=vif"]

unswizzleMac :: Mac -> Mac
unswizzleMac mac = let bytes = macToBytes mac
                       h     = (head bytes) .&. 253
                    in bytesToMac $ h : tail bytes

data Display = VNC | Other
-- Next section: information about PCI Passthrough Devices
pciSpecs :: VmConfig -> Rpc [PciSpec]
pciSpecs cfg = do
    let devices = vmcfgPciPtDevices cfg
        uuid    = vmcfgUuid cfg

    return $ ["pci=[" ++ (concat (intersperse "," (map (\dev -> "'" ++ stringAddr dev ++ "'") devices))) ++ "]"]
 where
   stringAddr (PciPtDev d _ _ _) =
           printf "%04x:%02x:%02x.%x"
               (pciDomain addr)
               (pciBus    addr)
               (pciSlot   addr)
               (pciFunc   addr)
       where addr = devAddr d

cpuidResponses :: VmConfig -> [String]
cpuidResponses cfg = map option (vmcfgCpuidResponses cfg) where
    option (CpuidResponse r) = printf "cpuid=%s" r

--helper function to wrap config options in quotes (xl specific)
wrapQuotes :: String -> String
wrapQuotes = (++"'") <$> ("'"++)

--helper function to wrap config option in brackets (xl specific)
wrapBrackets :: String -> String
wrapBrackets = (++"]") <$> ("["++)

--helper function to combine all extra_hvm args into one 'extra_hvm' entry
combineExtraHvmParams hvmStuff = ["device_model_args=[" ++ concat (intersperse "," (map wrapQuotes hvmStuff)) ++ "]"]

-- Additional misc stuff in xenvm config
miscSpecs :: VmConfig -> Rpc [Param]
miscSpecs cfg = do
    t        <- timeOffset
    v        <- videoram
    other    <- otherXenvmParams
    -- these bits depend on appropriate policy being set to true
    cdromA   <- policyQueryCdAccess uuid
    cdromR   <- policyQueryCdRecording uuid
    bsgs     <- liftIO $ getHostBSGDevices
    let cdexcl_opt = case vmcfgCdExclusive cfg of
                       True -> "-exclusive"
                       _    -> ""
    let cdromParams = let bsgList = map cdromParam bsgs in
                        case length bsgList of
                           0 -> []
                           _ -> (["-drive"] ++) $ intersperse "-drive" bsgList
        cdromParam (BSGDevice a b c d) =
            let bsg_str = "/dev/bsg/" ++ (concat . intersperse ":" $ map show [a,b,c,d]) in
            case (cdromA,cdromR) of
              -- no cdrom
              (False, _)    -> ""
              -- full access to cdrom
              (True, True)  -> printf "file=%s:%s,media=cdrom,if=atapi-pt,format=atapi-pt-fmt,readonly=%s" atapiType bsg_str "off"
              -- readonly access to cdrom
              (True, False) -> printf "file=%s:%s,media=cdrom,if=atapi-pt,format=atapi-pt-fmt,readonly=%s" atapiType bsg_str "on"
        atapiType = if (vmcfgStubdom cfg) then "atapi-pt-v4v" else "atapi-pt-local"

    let empty = pure []
    snd      <- ifM (policyQueryAudioAccess    uuid) sound          empty
    audioRec <- ifM (policyQueryAudioRecording uuid) empty          (pure ["-disable-audio-rec"])
    vcpus    <- readConfigPropertyDef uuid vmVcpus (1::Int)
    coresPS  <- readConfigPropertyDef uuid vmCoresPerSocket vcpus
    stubdom_ <- liftIO stubdom
    usb      <- usb_opts
    hpet_    <- hpet
    timer_mode_ <- timer_mode
    nested_ <- nested
    dm_override_ <- liftRpc dm_override
    extra_hvms <- readConfigPropertyDef uuid vmExtraHvms []

    let coresPSpms = if coresPS > 1 then ["cores_per_socket=" ++ show coresPS] else ["cores_per_socket=" ++ show vcpus]
    return $
           t ++ v ++ combineExtraHvmParams (cdromParams ++ audioRec ++ extra_hvms)
        ++ ["memory="++show (vmcfgMemoryMib cfg) ]
        ++ ["maxmem="++show (vmcfgMemoryStaticMaxMib cfg) ]
        ++ smbios_path ++ snd ++ coresPSpms
        ++ stubdom_ ++ cpuidResponses cfg ++ usb ++ platform ++ other               
        ++ hpet_
        ++ timer_mode_
        ++ nested_
        ++ dm_override_
        ++ acpi_path
    where
      uuid = vmcfgUuid cfg
      -- omit if not specified
      timeOffset = maybeToList . fmap ("time-offset="++) <$>
        readConfigProperty uuid vmTimeOffset
      -- 16 meg if not specified
      videoram  = do
        hvm <- readConfigPropertyDef uuid vmHvm False
        let defaultVideoram = if hvm then 16 else 0
        (\ram -> ["videoram="++ram]) . fromMaybe (show defaultVideoram) <$>
          readConfigProperty uuid vmVideoram
      hpet = (i <$> readConfigPropertyDef uuid vmHpet vmHpetDefault) >>= \ v -> return ["hpet=" ++ show v]
             where i True = 1
                   i _    = 0
      timer_mode = readConfigPropertyDef uuid vmTimerMode vmTimerModeDefault >>= 
                   \ v -> return ["timer_mode=" ++ (show v)]
      nested = readConfigPropertyDef uuid vmNestedHvm False >>=
                   \ v -> if v then return ["nested=true"] else return []

      smbios_path =
          case (vmcfgSmbios cfg) of
            [] -> []
            smbiosPath -> [ "smbios_firmware='" ++ smbiosPath ++ "'" ]

      acpi_path =
          case (vmcfgAcpi cfg) of
            [] -> []
            acpiPath   -> [ "acpi_firmware='" ++ acpiPath ++ "'" ]

      -- Activate sound
      sound = maybeToList . fmap (("soundhw='"++) <$> (++"'")) <$> readConfigProperty uuid vmSound

      -- Tells xl to use a stubdom or not
      stubdom | not (vmcfgStubdom cfg) = return []
              | otherwise              = return ["device_model_stubdomain_override=1"]

      -- Specifies path to qemu binary
      dm_override =
        do
           hvm <- readConfigPropertyDef uuid vmHvm False
           case hvm of
             False     -> return []
             otherwise -> return ["device_model_override='/usr/bin/qemu-system-i386'"]

      usb_opts | not (vmcfgUsbEnabled cfg) = return ["usb=0"]
               | otherwise                 = return []

      platform =
                  x (vmcfgRestrictDisplayDepth cfg) "restrictdisplaydepth="
               ++ x (vmcfgRestrictDisplayRes   cfg) "restrictdisplayres="
               where x cond s = if cond then [s++"1"] else [s++"0"]

      -- Other config keys taken directly from .config subtree which we delegate directly
      -- to xenvm
      passToXenvmProperties =
          [ ("hvm"             , vmHvm)
          , ("pae"             , vmPae)
          , ("acpi"            , vmAcpi)
          , ("apic"            , vmApic)
          , ("viridian"        , vmViridian) --set to 'default'
          , ("nx"              , vmNx)
          , ("dm_display"      , vmDisplay) --this should now be set to surfman or none
          , ("boot"            , vmBoot)
          , ("extra"           , vmCmdLine)
          , ("vcpus"           , vmVcpus)
          , ("hap"             , vmHap)
          , ("vkb"             , vmVkbd)
          , ("vfb"             , vmVfb)
          , ("seclabel"        , vmFlaskLabel)
          , ("serial"          , vmSerial)
          , ("stubdom_cmdline" , vmStubdomCmdline)
          , ("stubdom_memory"  , vmStubdomMemory)
          ]

      -- xl config handles certain options different than others (eg. quotes, brackets)
      -- we format them on a case by case basis here before sending them off to xl.
      otherXenvmParams = concat <$> sequence
                         [ reverse . catMaybes <$> mapM g passToXenvmProperties
                         , extra_xenvm
                         ]
        where g (name,prop) = fmap (\v ->
                              case v of
                                "none"  -> []
                                "true"  -> name ++ "=" ++ "1"
                                "false" -> name ++ "=" ++ "0"
                                _       -> case name of
                                             "viridian" -> name ++ "=" ++ (wrapBrackets $ wrapQuotes v)
                                             "serial"   -> name ++ "=" ++ (wrapBrackets $ wrapQuotes v)
                                             "extra"    -> case v of
                                                           "" -> []
                                                           _  -> name ++ "=" ++ (wrapQuotes v)
                                             "seclabel" -> name ++ "=" ++ (wrapQuotes v)
                                             "dm_display" -> name ++ "=" ++ (wrapQuotes v)
                                             "boot"     -> name ++ "=" ++ (wrapQuotes v)
                                             "stubdom_cmdline" -> name ++ "=" ++ (wrapQuotes v)
                                             _          -> name ++ "=" ++ v) <$>
                                readConfigProperty uuid prop

              -- additional parameters passed through config/extra-xenvm/... key
              extra_xenvm :: Rpc [Param]
              extra_xenvm = readConfigPropertyDef uuid vmExtraXenvm []
