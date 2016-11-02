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

{-# LANGUAGE PatternGuards, ScopedTypeVariables, FlexibleContexts #-}
module Vm.Queries
               (
                 getDomainID
               , getDomainUuid
               , getStubDomainID
               , whenDomainID, whenDomainID_
               , getFocusVm
               , getVms
               , getVmsBy
               , getVmsByType
               , getVmByDomid
               , getVmShutdownOrder
               , getGuestVms
               , getRunningHDX
               , getGraphicsFallbackVm
               , getDefaultNetworkBackendVm
               , getConfigCorruptionInfo
               , getNic
               , getVmNicDefs
               , getVmNicDefs'
               , getVmWiredNics
               , getVmWirelessNics
               , getVmNicMacActual
               , getVmDiskEncryptionKeySet
               , getVmDiskVirtualSizeMB
               , getVmDiskPhysicalUtilizationBytes
               , getNicIds
               , getCdroms
               , getDisks
               , getDisk
               , getDiskWithPath
               , envIsoDir
               , envIsoPath
               , pickDiskVirtPath
               , getVmPrivateSpaceUsedMiB
               , getCryptoKeyLookupPaths
               , getPciPtRules
               , getPciPtDevices
               , getVisibleVms
               , whenVmRunning
               , isRunning
               , isLoginRequired
               , isManagedVm
               , whenManagedVm
               , countRunningVm
               , backendNode
               , getVmBedOperation
               , getVmConfig
               , getVmIconBytes
               , getSeamlessVms
                 -- property accessors
               , getVmType, getVmGraphics, getMaxVgpus
               , getVmWiredNetwork, getVmWirelessNetwork, getVmGpu, getVmCd, getVmMac, getVmAmtPt, getVmPorticaEnabled, getVmPorticaInstalled
               , getVmSeamlessTraffic, getVmAutostartPending, getVmHibernated, getVmMemoryStaticMax
               , getVmMemoryMin
               , getVmMemoryTargetKib
               , getVmMemoryTarget, getVmStartOnBoot, getVmHiddenInSwitcher, getVmHiddenInUi, getVmMemory, getVmName
               , getVmImagePath, getVmSlot, getVmPvAddons, getVmPvAddonsVersion
               , getVmTimeOffset, getVmCryptoUser, getVmCryptoKeyDirs, getVmAutoS3Wake
               , getVmNotify, getVmHvm, getVmPae, getVmApic, getVmAcpi, getVmViridian, getVmNx, getVmSound, getVmDisplay
               , getVmBoot, getVmCmdLine, getVmKernel, getVmInitrd, getVmAcpiPath, getVmVcpus, getVmCoresPerSocket
               , getVmKernelPath
               , getVmKernelExtract
               , getVmInitrdExtract
               , getVmVideoram, getVmPassthroughMmio, getVmPassthroughIo, getVmFlaskLabel
               , getVmAcpiState, getVmHap, getVmSmbios, getVmDescription, getVmMeasured
               , getVmExtraXenvm, getVmExtraHvm
               , getVmStartOnBootPriority, getVmKeepAlive, getVmProvidesNetworkBackend
               , getVmShutdownPriority, getVmProvidesGraphicsFallback
               , getVmSeamlessId, getVmStartFromSuspendImage
               , getVmQemuDmPath, getVmQemuDmTimeout
               , getVmDependencies
               , getVmTrackDependencies
               , getVmSeamlessMouseLeft, getVmSeamlessMouseRight
               , getVmOs, getVmControlPlatformPowerState, getVmGreedyPcibackBind
               , getVmFirewallRules, getVmOemAcpiFeatures, getVmUsbEnabled, getVmUsbAutoPassthrough, getVmUsbControl, getVmCpuid, getVmCpuidResponses
               , getVmStubdom, getVmStubdomMemory, getVmStubdomCmdline
               , getVmRunPostCreate, getVmRunPreDelete, getVmRunOnStateChange, getVmRunOnAcpiStateChange
               , getVmRunPreBoot
               , getVmRunInsteadofStart
               , getVmUsbGrabDevices
               , getVmNativeExperience, getVmShowSwitcher, getVmWirelessControl
               , getVmXciCpuidSignature
               , getVmS3Mode
               , getVmS4Mode
               , getVmVsnd
               , getVmRealm
               , getVmSyncUuid
               , getVmIcbinnPath
               , getVmOvfTransportIso
               , getVmDownloadProgress
               , getVmReady
               , getVmProvidesDefaultNetworkBackend
               , getVmVkb
               , getVmVfb
               , getVmV4V
               , getVmRestrictDisplayDepth
               , getVmRestrictDisplayRes
               , getVmPreserveOnReboot
               , getVmBootSentinel
               , getVmHpet
               , getVmTimerMode
               , getVmNestedHvm
               , getVmSerial
               ) where

import Data.String
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Arrow
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import Directory
import Text.Printf

import XenMgr.Db
import XenMgr.Errors
import Vm.Types
import Vm.DomainCore
import Vm.Dm
import Vm.Config
import Vm.Pci
import Vm.DepGraph
import Vm.Policies
import Vm.ProductProperty
import qualified Vm.V4VFirewall as Firewall
import Tools.Misc
import Tools.Process
import Tools.Text
import Tools.Log
import Tools.Future
import Tools.IfM

import System.FilePath
import System.Posix.Files (fileSize, getFileStatus)
import Tools.XenStore

import XenMgr.Rpc
import XenMgr.Connect.NetworkDaemon
import XenMgr.Config
import XenMgr.Errors
import XenMgr.Host
import XenMgr.Connect.Xl (isRunning)
import qualified XenMgr.Connect.Xl as Xl
import Rpc.Autogen.SurfmanClient

import Data.Bits
import System.IO.Unsafe

-- All VMS in database
allVms :: MonadRpc e m => m [Uuid]
allVms = map fromString <$> dbList "/vm"

-- VMS with corrupt configs
getConfigCorruptionInfo :: Rpc [(Uuid,String)]
getConfigCorruptionInfo = do
    vms  <- allVms
    cfgs <- mapM (\uuid -> getVmConfig uuid False) vms
    return $ foldl' f [] cfgs
  where
    f acc cfg =
        case diagnose cfg of
          []   ->  acc
          p:ps -> (vmcfgUuid cfg,p) : acc

-- VMS with correct configs
-- UPDATE: removed the correctness check, it was slowing the RPC extremely and does not protect
-- against anything interesting anyway
correctVms :: MonadRpc e m => m [Uuid]
correctVms = allVms

whenVmRunning :: (MonadRpc e m) => Uuid -> m () -> m ()
whenVmRunning uuid f = go =<< isRunning uuid
    where go True = f
          go _    = return ()

-- iterate over configured devices and plug proper backend domid
plugBackendDomains :: VmConfig -> Rpc VmConfig
plugBackendDomains cfg =
    do plugged <- mapM plugNic (vmcfgNics cfg)
       return $ cfg { vmcfgNics = plugged }
  where
    -- plug a nic to backend by resolving uuid if configured so, otherwise
    -- use global network domain id (which is dom0 or ndvm)
    plugNic :: NicDef -> Rpc NicDef
    plugNic nic = backendFromUuid nic =<< getVmNicBackendUuid nic
        where
          backendFromUuid nic Nothing = return nic
          backendFromUuid nic (Just uuid) = do
            domid <- getDomainID uuid
            return $ nic { nicdefBackendDomid = domid }

getMaxVgpus :: Rpc Int
getMaxVgpus = vgpu <$> querySurfmanVgpuMode
    where vgpu Nothing  = 0
          vgpu (Just v) = vgpuMaxVGpus v

-- prepare the vm config
getVmConfig :: Uuid -> Bool -> Rpc VmConfig
getVmConfig uuid resolve_backend_uuids =
    do disks <- future $ getVmStartupDisks uuid
       let setupGeneratedMac uuid nic
             | isNothing (nicdefMac nic) = nic { nicdefMac = Just (generatedVmNicMac uuid $ nicdefId nic) }
             | otherwise                 = nic
       nics  <- future $ (map (setupGeneratedMac uuid) <$> getVmNicDefs' uuid)
       nets  <- future $ getAvailableVmNetworks =<< force nics
       key_dirs <- future $ getVmCryptoKeyDirs uuid --vm config only needs crypto dirs set for the vm, if any
       pcis <- future $ getPciPtDevices uuid
       qemu <- future $ getVmQemuDmPath uuid
       qemu_timeout <- future $ getVmQemuDmTimeout uuid
       excl_cd <- future $ policyQueryCdExclusive
       vgpu <- future $ ifM (getVmHvm uuid) querySurfmanVgpuMode (return Nothing)
       gfx <- future $ getVmGraphics uuid
       oem_acpi <- future $ getVmOemAcpiFeatures uuid
       pv_addons <- future $ getVmPvAddons uuid
       autostart <- future $ getVmStartOnBoot uuid
       seamless <- future $ getVmSeamlessTraffic uuid
       smbios_path <- future $ getVmSmbios uuid
       acpi_path <- future $ getVmAcpiPath uuid
       stubdom <- future $ getVmStubdom uuid
       stubdom_memory <- future $ getVmStubdomMemory uuid
       stubdom_cmdline <- future $ Just <$> getVmStubdomCmdline uuid
       os <- future $ getVmOs uuid
       cpuidresps <- future $ getVmCpuidResponses uuid
       xcisig <- future $ getVmXciCpuidSignature uuid
       usb <- future $ getVmUsbEnabled uuid
       auto_passthrough <- future $ getVmUsbAutoPassthrough uuid
       v <- future $ getHostXcVersion
       name <- future $ domain_name
       mem <- future $ getVmMemory uuid
       memmin <- future $ getVmMemoryMin uuid
       memmax <- future $ getVmMemoryStaticMax uuid
       kernel <- future $ getVmKernelPath uuid
       rdd <- future $ getVmRestrictDisplayDepth uuid
       rds <- future $ getVmRestrictDisplayRes uuid
       preserve_on_reboot <- future $ getVmPreserveOnReboot uuid
       cfg <-
           force $ VmConfig
                     <$> pure uuid
                     <*> name
                     <*> qemu
                     <*> qemu_timeout
                     <*> kernel
                     <*> cpuidresps
                     <*> xcisig
                     <*> os
                     <*> nics
                     <*> disks
                     <*> nets
                     <*> key_dirs
                     <*> pv_addons
                     <*> gfx
                     <*> rdd
                     <*> rds
                     <*> oem_acpi
                     <*> vgpu
                     <*> pcis
                     <*> excl_cd
                     <*> autostart
                     <*> seamless
                     <*> smbios_path
                     <*> acpi_path
                     <*> v
                     <*> usb
                     <*> auto_passthrough
                     <*> stubdom
                     <*> stubdom_memory
                     <*> stubdom_cmdline
                     <*> mem
                     <*> memmin
                     <*> memmax
                     <*> preserve_on_reboot
       if resolve_backend_uuids
          then plugBackendDomains cfg
          else return cfg
    where
      domain_name = of_type =<< getVmType uuid where
          of_type Svm = return Nothing
          of_type _   = Just <$> getVmName uuid

pickDiskVirtPath :: MonadRpc e m => Uuid -> m String
pickDiskVirtPath uuid = nextFreeVirtPath . M.elems <$> getDisks uuid

-- choose a new available virtual path for disk
nextFreeVirtPath :: [Disk] -> String
nextFreeVirtPath disks = head . filter (not_taken disks) $ candidates
  where
    candidates = [ "hda", "hdb", "hdc", "hdd" ] ++ map (\s -> "xvd" ++ [s]) ['a'..'z']
    not_taken disks virtp = not $ virtp `elem` (map diskDevice disks)

envIsoDir :: FilePath
envIsoDir = "/var/lib/ovf"

envIsoPath :: Uuid -> FilePath
envIsoPath uuid = envIsoDir ++ "/" ++ show uuid ++ "-env.iso"

-- disks used to boot this vm
-- add ovf environment disk here if necessary for the specified transport mode
getVmStartupDisks :: Uuid -> Rpc [Disk]
getVmStartupDisks uuid = do
  enviso <- getVmOvfTransportIso uuid
  disks <- readConfigPropertyDef uuid vmDisks []
  let vpath = nextFreeVirtPath disks
  return $ if enviso then envDisk vpath : disks else disks
  where
    envDisk vpath =
      Disk { diskPath = envIsoPath uuid
           , diskType = DiskImage
           , diskMode = ReadOnly
           , diskEnabled = True
           , diskDevice = vpath
           , diskDeviceType = DiskDeviceTypeCdRom
           , diskSnapshotMode = Nothing
           , diskSha1Sum = Nothing
           , diskShared = False
           , diskManagedType = UnmanagedDisk }
      
getVms :: (MonadRpc e m) => m [Uuid]
getVms = correctVms

getVmsBy :: (MonadRpc e m) => (Uuid -> m Bool) -> m [Uuid]
getVmsBy f = getVms >>= filterM f

getVmType :: (MonadRpc e m) => Uuid -> m VmType
getVmType uuid = readConfigPropertyDef uuid vmType Svm

getVmKernelPath :: (MonadRpc e m) => Uuid -> m (Maybe FilePath)
getVmKernelPath uuid = do
  hvm <- getVmHvm uuid
  if hvm then return Nothing else do
    p <- getVmKernel uuid
    if p /= "" then return (Just p) else return (Just $ "/tmp/kernel-"++show uuid)

getVmNicBackendUuid :: NicDef -> Rpc (Maybe Uuid)
getVmNicBackendUuid nic =
     case (nicdefBackendUuid nic, nicdefBackendName nic) of
          (Nothing  , Nothing)   -> defaultNetUuid
          (Just uuid, _      )   -> return $ Just uuid
          (Nothing  , Just name) ->
            do vms <- getVmsBy (\vm -> (== name) <$> getVmName vm)
               case vms of
                 (uuid:_) -> return $ Just uuid
                 _        -> return Nothing
     where
       -- default network backend uuid (if not overriden by nic config) comes from network daemon query
       defaultNetUuid = from =<< getNetworkBackend' (nicdefNetwork nic) where
         from (Just vm) = return (Just vm)
         from Nothing   = getDefaultNetworkBackendVm

getAvailableVmNetworks :: [NicDef] -> Rpc [NetworkInfo]
getAvailableVmNetworks [] = return []
getAvailableVmNetworks nics
    = do all_networks <- listNetworks
         let nic_networks = map nicdefNetwork (filter nicdefEnable nics)
         catMaybes <$> (mapM statNetwork $ intersect all_networks nic_networks)

getDependencyGraph :: Rpc (DepGraph Uuid)
getDependencyGraph =
    do vms <- getVms
       edges <- concat <$> mapM edge vms
       return $ depgraphFromEdges edges
    where
      edge uuid =
          do nics <- getVmNicDefs' uuid
             net_backends <- nub . catMaybes <$> mapM getVmNicBackendUuid nics
             return $ map (\dep -> (uuid,dep)) net_backends

getVmDependencies :: Uuid -> Rpc [Uuid]
getVmDependencies uuid =
    do graph <- getDependencyGraph
       case dependencies uuid graph of
         Nothing -> error "errors in dependency graph; check for cycles"
         Just xs -> return xs

getVmTrackDependencies :: Uuid -> Rpc Bool
getVmTrackDependencies uuid = readConfigPropertyDef uuid vmTrackDependencies False

getVmSeamlessMouseX :: Uuid -> (String -> Rpc [HostGpu]) -> Rpc (Maybe Uuid)
getVmSeamlessMouseX uuid gpusNextTo =
    do gpu <- gpu_id <$> getVmGpu uuid
       adjacent_gpus <- map gpuId <$> ( gpusNextTo gpu `catchError` (\ex -> return []))
       return . first
         =<< filterM isRunning
         =<< return . concat
         =<< mapM with_gpu adjacent_gpus
    where
      gpu_id "" = "hdx"
      gpu_id  x = x
      first [] = Nothing
      first (uuid:_) = Just $ uuid
      with_gpu "hdx" = getVisibleVms
      with_gpu gpu   = getVmsBy (\vm -> (== gpu) <$> getVmGpu vm)

getVmSeamlessMouseLeft :: Uuid -> Rpc Int
getVmSeamlessMouseLeft uuid = from =<< getVmSeamlessMouseX uuid getGpusLeftOf where
    from Nothing   = return (-1)
    from (Just vm) | vm == uuid = return (-1)
    from (Just vm) = getVmSlot vm

getVmSeamlessMouseRight :: Uuid -> Rpc Int
getVmSeamlessMouseRight uuid = from =<< getVmSeamlessMouseX uuid getGpusRightOf where
    from Nothing   = return (-1)
    from (Just vm) | vm == uuid = return (-1)
    from (Just vm) = getVmSlot vm

getVmOs :: Uuid -> Rpc SupportedOS
getVmOs uuid = readConfigPropertyDef uuid vmOs "" >>= return . fromMaybe UnknownOS . osFromStr

getVmOemAcpiFeatures :: Uuid -> Rpc Bool
getVmOemAcpiFeatures uuid = readConfigPropertyDef uuid vmOemAcpiFeatures False

getVmUsbEnabled :: Uuid -> Rpc Bool
getVmUsbEnabled uuid = readConfigPropertyDef uuid vmUsbEnabled True

getVmUsbAutoPassthrough :: Uuid -> Rpc Bool
getVmUsbAutoPassthrough uuid = readConfigPropertyDef uuid vmUsbAutoPassthrough True

getVmUsbControl :: Uuid -> Rpc Bool
getVmUsbControl uuid = readConfigPropertyDef uuid vmUsbControl False

getVmStubdom :: Uuid -> Rpc Bool
getVmStubdom uuid = readConfigPropertyDef uuid vmStubdom False

getVmStubdomMemory :: Uuid -> Rpc Int
getVmStubdomMemory uuid = readConfigPropertyDef uuid vmStubdomMemory 80

getVmStubdomCmdline :: Uuid -> Rpc String
getVmStubdomCmdline uuid = readConfigPropertyDef uuid vmStubdomCmdline ""

getVmCpuid :: Uuid -> Rpc String
getVmCpuid uuid = readConfigPropertyDef uuid vmCpuid ""

getVmCpuidResponses :: Uuid -> Rpc [CpuidResponse]
getVmCpuidResponses uuid = map CpuidResponse . filter (not . null) . map strip . split ';' <$> getVmCpuid uuid

getVmXciCpuidSignature :: Uuid -> Rpc Bool
getVmXciCpuidSignature uuid = readConfigPropertyDef uuid vmXciCpuidSignature False

getVmsByType :: VmType -> Rpc [Uuid]
getVmsByType t =
    getVmsBy equalTypes
  where
    equalTypes uuid = getVmType uuid  >>= return . (== t)

getVmByDomid :: DomainID -> Rpc (Maybe Uuid)
getVmByDomid domid =
    do vms <- getVms
       domids <- mapM getDomainID vms
       case filter matches (zip vms domids) of
         ((uuid,_) : _) -> return $ Just uuid
         _              -> return Nothing
    where
      matches (uuid,Just domid') = domid == domid'
      matches _ = False

groupVmsBy :: (Eq a) => (Uuid -> Rpc a) -> [Uuid] -> Rpc [[Uuid]]
groupVmsBy get_property uuids =
    do props <- mapM get_property uuids
       let vs = zip uuids props
           rs = groupBy (\a b -> snd a == snd b) vs
       return . map (map fst) $ rs

-- get the vm uuid which is supposed to have focus
getFocusVm :: Rpc (Maybe Uuid)
getFocusVm =
    do maybe_uuid <- liftIO $ xsRead "/local/domain/0/switcher/focus-uuid"
       return . fmap fromString $ maybe_uuid

-- get the visible domains (surfman query)
-- NOTE: this doesn't include vms which surfman does not know about, i.e. something which
-- uses passthrough GPU directly
getVisibleDomids :: Rpc [DomainID]
getVisibleDomids
  = map fromIntegral <$>
     comCitrixXenclientSurfmanGetVisible "com.citrix.xenclient.surfman" "/" `catchError` \_ -> return []

getVisibleVms :: Rpc [Uuid]
getVisibleVms = catMaybes <$> (mapM getDomainUuid =<< getVisibleDomids)

-- shutdown by descending priority, in parallel if priority equal
getVmShutdownOrder :: Rpc [[Uuid]]
getVmShutdownOrder =
    do vms   <- getVms
       pris  <- mapM getVmShutdownPriority vms
       let order  = reverse . sortBy (comparing snd) $ zip vms pris
           order' = groupBy (\a b -> snd a == snd b) order
       return . map (map fst) $ order'

getGuestVms :: Rpc [Uuid]
getGuestVms =
    getVmsBy is_guest
    where
      match_guest_type Svm = True
      match_guest_type _   = False
      is_guest uuid = match_guest_type <$> getVmType uuid

getRunningHDX :: Rpc [Uuid]
getRunningHDX = do
    filterM isRunning =<< getVmsBy isHDX
    where
      isHDX uuid = getVmGraphics uuid >>= return . (== HDX)

getGraphicsFallbackVm :: Rpc (Maybe Uuid)
getGraphicsFallbackVm = do
  vms <- getVmsBy getVmProvidesGraphicsFallback
  case vms of
    (vm : _) -> return $ Just vm
    _        -> return Nothing

getDefaultNetworkBackendVm :: Rpc (Maybe Uuid)
getDefaultNetworkBackendVm = do
  vms <- getVmsBy getVmProvidesDefaultNetworkBackend
  case vms of
    (vm : _) -> return $ Just vm
    _        -> return Nothing

-- Query for ACPI state, will return 0,3,4, or 5
getVmAcpiState :: Uuid -> Rpc Int
getVmAcpiState uuid = do
    -- xenvm acpiState query is bit flaky as it can only be counted to return meaningful
    -- value of 3 or 0 (as it just returns the hypercall query value directly). Rest has to be
    -- derived from magic ball

    -- xen 4.1: hypercall to get acpi state doesn't work on pv domains nor shutdown vms anymore
    pv <- not <$> getVmHvm uuid
    running <- isRunning uuid
    ll_acpi_state <- if pv
                        then return 0
                        else (
                          if not running
                             then return 5
                             else liftIO $ Xl.acpiState uuid
                             )
    deriveFrom <$> (liftIO $ Xl.state uuid)
               <*> return ll_acpi_state
               <*> readConfigPropertyDef uuid vmHibernated False
  where
    deriveFrom :: VmState -> Int -> Bool -> Int
    deriveFrom Shutdown   _ False = 5
    deriveFrom Shutdown   _ True  = 4
    deriveFrom _          3 _     = 3
    deriveFrom _          0 _     = 0
    deriveFrom _          _ _     = 0

-- Get nics from database
getVmNicDefs :: MonadRpc e m => Uuid -> m NicDefMap
getVmNicDefs uuid = readConfigPropertyDef uuid vmNics M.empty

getVmNicDefs' :: MonadRpc e m => Uuid -> m [NicDef]
getVmNicDefs' uuid = sortBy (comparing nicdefId) . M.elems <$> getVmNicDefs uuid

-- Get just nic ids from database
getNicIds :: MonadRpc e m => Uuid -> m [NicID]
getNicIds uuid = map nicdefId <$> getVmNicDefs' uuid

getVmWiredNics, getVmWirelessNics :: MonadRpc e m => Uuid -> m [NicDef]
getVmWiredNics    vm  = filter (not . nicdefWirelessDriver) <$> getVmNicDefs' vm
getVmWirelessNics vm  = filter (      nicdefWirelessDriver) <$> getVmNicDefs' vm

getNic :: MonadRpc e m => Uuid -> NicID -> m (Maybe NicDef)
getNic uuid id = readConfigProperty uuid (vmNic id)

getCryptoKeyLookupPaths :: Uuid -> Rpc [FilePath]
getCryptoKeyLookupPaths uuid =
    do user <- getVmCryptoUser uuid
       vm_dirs <- split ',' <$> getVmCryptoKeyDirs uuid
       platform_dirs <- split ',' <$> appGetPlatformCryptoKeyDirs
       let dirs = filter (not . null) $ [user_key_dir user] ++ vm_dirs ++ platform_dirs
       liftIO $ filterM doesDirectoryExist dirs
    where
      user_key_dir "" = ""
      user_key_dir user = "/config/sec/s-" ++ user

getDisks :: MonadRpc e m => Uuid -> m DiskMap
getDisks uuid =
    readConfigPropertyDef uuid vmDisks M.empty

getDisk :: MonadRpc e m => Uuid -> DiskID -> m (Maybe Disk)
getDisk uuid diskID = readConfigProperty uuid (vmDisk diskID)

getDisk' :: MonadRpc XmError m => Uuid -> DiskID -> m Disk
getDisk' uuid diskID = getDisk uuid diskID >>= \disk -> case disk of
  Just d -> return d
  Nothing-> failNoSuchDisk

getDiskWithPath :: MonadRpc e m => Uuid -> FilePath -> m (Maybe Disk)
getDiskWithPath uuid p = sh . filter ((== p) . diskPath) . M.elems <$> getDisks uuid where
  sh (x:_) = Just x
  sh _ = Nothing

orElseIfException :: IO a -> IO a -> IO a
orElseIfException f g =
  f `E.catch` ( \(_ :: E.SomeException) -> g )

getVmDiskEncryptionKeySet :: MonadRpc XmError m => Uuid -> DiskID -> m Bool
getVmDiskEncryptionKeySet uuid disk_id =
  do disk <- getDisk' uuid disk_id
     if (diskType disk == VirtualHardDisk)
        then from <$> liftIO (readKey disk)
        else return False     
  where
    from Nothing = False
    from (Just "none") = False
    from _ = True
    readKey disk = (Just . chomp <$> readProcessOrDie "vhd-util" ["key", "-p", "-n", diskPath disk] "")
                                 `orElseIfException` return Nothing
                   
getVmDiskVirtualSizeMB :: MonadRpc XmError m => Uuid -> DiskID -> m Integer
getVmDiskVirtualSizeMB uuid disk_id =
  do disk <- getDisk' uuid disk_id
     liftIO $
       if (diskType disk == VirtualHardDisk)
         then (read . chomp <$> readProcessOrDie "vhd-util" ["query", "-v", "-n", diskPath disk] "")
              `orElseIfException` return 0
         else return 0
     
getVmDiskPhysicalUtilizationBytes :: MonadRpc XmError m => Uuid -> DiskID -> m Integer
getVmDiskPhysicalUtilizationBytes uuid disk_id =
  do disk <- getDisk' uuid disk_id
     liftIO $ 
       if (diskType disk == VirtualHardDisk)
         then (read . chomp <$> (readProcessOrDie "vhd-util" ["query", "-s", "-n", diskPath disk] ""))
              `orElseIfException` return 0
         else return 0

getVmPrivateSpaceUsedMiB :: Uuid -> Rpc Int
getVmPrivateSpaceUsedMiB uuid =
  (M.elems <$> getDisks uuid) >>= mapM diskUsed >>= return . sum where
    diskUsed d =
      case diskType d of
        DiskImage -> fileSz (diskPath d)
        QemuCopyOnWrite -> fileSz (diskPath d)
        VirtualHardDisk -> fileSz (diskPath d)
        ExternalVdi -> fileSz (diskPath d)
        Aio -> fileSz (diskPath d)
        Raw -> fileSz (diskPath d)
        PhysicalDevice -> return 0 -- TODO
    fileSz  f = either (const 0) id <$> liftIO (E.try $ fileSz' f :: IO (Either E.SomeException Int))
    fileSz' f = fromIntegral . mib . fileSize <$> getFileStatus f
    mib = (`div` (1024*1024))

-- Get cdrom definitions from database
getCdroms :: Uuid -> Rpc [Disk]
getCdroms uuid =
    getDisks uuid >>= pure . filter isCdrom . M.elems

-- Get passthrough rules for a VM
getPciPtRules :: Uuid -> Rpc PciPtRuleMap
getPciPtRules uuid =
    readConfigPropertyDef uuid vmPcis M.empty

getPciPtRulesList :: Uuid -> Rpc [PciPtRule]
getPciPtRulesList uuid =
    map snd . M.toList <$> getPciPtRules uuid

-- Get passthrough devices for a VM
getPciPtDevices :: Uuid -> Rpc [PciPtDev]
getPciPtDevices uuid =
    do gfx <- getVmGraphics uuid
       amt <- amtPtActive uuid
       -- We've got PCI rules for a specifc VM
       vm_rules <- getPciPtRulesList uuid
       -- We've got PCI rules for HDX
       hdx_devs <- case gfx of
                     HDX -> querySurfmanVgpuMode >>= return . vgpu_devs
                     _   -> return []
       -- And then there are AMT passthrough rules
       let amt_rules = case amt of True -> amtPciPtRules
                                   _    -> []
       -- And optional secondary gpu devs based on GPU property
       -- ('hdx' indicates surfman devices and should be ignored here)
       gpu_addr_str <- readConfigPropertyDef uuid vmGpu ""
       let gpu_addr = case gpu_addr_str of "hdx" -> Nothing
                                           _     -> pciFromStr gpu_addr_str
       gpu_dev  <- liftIO $ case gpu_addr of
                     Nothing   -> return []
                     Just addr -> do dev <- pciGetDevice addr
                                     return [PciPtDev dev PciSlotDontCare False SourceConfig]

       let rules = amt_rules ++ vm_rules

       devs_from_rules <- liftIO $ pciGetMatchingDevices SourceConfig rules
       return $ hdx_devs ++ gpu_dev ++ devs_from_rules
    where
      vgpu_devs Nothing = []
      vgpu_devs (Just mode) = vgpuPciPtDevices mode

getVmFirewallRules :: Uuid -> Rpc [Firewall.Rule]
getVmFirewallRules uuid = readConfigPropertyDef uuid vmFirewallRules []

isLoginRequired :: Uuid -> Rpc Bool
isLoginRequired uuid = do
    user <- readConfigProperty uuid vmCryptoUser
    case user of
      Nothing   -> return False
      Just uid  -> liftIO $ notMounted uid
  where
    notMounted uid = return . isNothing =<< spawnShell' ("grep -q \"/config/sec/s-" ++ uid ++ "\" /proc/mounts")

isManagedVm :: MonadRpc e m => Uuid -> m Bool
isManagedVm uuid =
    dbExists $ "/vm/" ++ show uuid ++ "/backend"

whenManagedVm :: (MonadRpc e m) => Uuid -> m () -> m ()
whenManagedVm uuid f = whenM ( isManagedVm uuid ) f

-- count how many vms of given type are running
countRunningVm :: VmType -> Rpc Int
countRunningVm typ = length <$> (filterM running =<< getVmsByType typ)
    where
      running uuid = Xl.isRunning uuid

getVmIconBytes :: Uuid -> Rpc B.ByteString
getVmIconBytes uuid =
    do icon_p <- getVmImagePath uuid
       let path = html_root </> icon_p
       liftIO $ B.readFile path
    where
      html_root = "/usr/lib/xui"

backendNode = "/local/domain/0/backend-domid"

-- wired network property returns bridge of first wired nic
getVmWiredNetwork :: Uuid -> Rpc (Maybe Network)
getVmWiredNetwork uuid
    = getVmWiredNics uuid >>= return . first
    where
      first []    = Nothing
      first (n:_) = Just $ nicdefNetwork n

-- wireless network property returns bridge of first wireless nic
getVmWirelessNetwork :: Uuid -> Rpc (Maybe Network)
getVmWirelessNetwork uuid
    = getVmWirelessNics uuid >>= return . first
    where
      first []    = Nothing
      first (n:_) = Just $ nicdefNetwork n

getVmGpu :: MonadRpc e m => Uuid -> m String
getVmGpu uuid
  = ifM (getVmNativeExperience uuid)
      hdxIfTools {- else -} current
  where
    current = readConfigPropertyDef uuid vmGpu ""
    hdxIfTools
      = ifM (getVmPvAddons uuid)
          (return "hdx") {- else -} current

getVmGraphics :: MonadRpc e m => Uuid -> m VmGraphics
getVmGraphics uuid =
    do gpu <- getVmGpu uuid
       return $ case gpu of
                  "hdx" -> HDX
                  _ -> VGAEmu

-- cd inserted in virtual drive
getVmCd :: Uuid -> Rpc String
getVmCd uuid =
    getCdroms uuid >>= pure . fromFirst
  where
    fromFirst []           = ""
    fromFirst (c:_)        = fromIsoName (takeFileName $ diskPath c)
    fromIsoName "null.iso" = ""
    fromIsoName other      = other

-- mac of first nic is considered to be 'VM mac'
getVmMac :: Uuid -> Rpc String
getVmMac uuid =
  getVmNicDefs' uuid >>= firstmac where
    firstmac []    = return ""
    firstmac (n:_) = getVmNicMacActual uuid (nicdefId n)

-- mac as it will be setup on vm, which is either user specified or auto-generated
getVmNicMacActual :: Uuid -> NicID -> Rpc String
getVmNicMacActual uuid nicid
  = from_nic =<< getNic uuid nicid
  where
    from_nic Nothing    = failNoSuchNic
    from_nic (Just nic) = return $ fromMaybe (generatedVmNicMac uuid nicid) $ nicdefMac nic

-- mac generation algo
generatedVmNicMac :: Uuid -> NicID -> String
generatedVmNicMac uuid nicid
  = intercalate ":"  . map (printf "%02x") . unicast_local $ hexs
  where
    hexs  = map f [0,2,1,7,6,4] where
      f i =     ((hdigest !! i     ) `shiftL` 4)
            .|.  (hdigest !! (i+12))
    unicast_local []     = []
    unicast_local (x:xs) = (0x2 .|. (x .&. 0xFE)) : xs
    halfbytes []         = []
    halfbytes (x:xs)     = (x .&. 0xF0) `shiftR` 4 : (x .&. 0x0F) : halfbytes xs
    hdigest              = halfbytes (md5 base)
    base                 = show nicid ++ " " ++ show uuid

-- yeah yeah we should do it using haskell lib, i know..
md5 :: String -> [Word8]
md5 x
  = parse . head . words . unsafePerformIO $ readProcessOrDie "md5sum" [] x
  where
    parse (a:b:xs) = byte a b : parse xs
    parse _ = []
    byte a b = read ('0' : 'x' : a : b : [])

getVmAmtPt :: Uuid -> Rpc Bool
getVmAmtPt uuid = readConfigPropertyDef uuid vmAmtPt False

-- '0' if not installed, '1' if installed and enabled, '2' if installed but disabled
getVmPorticaEnabled :: Uuid -> Rpc Int
getVmPorticaEnabled uuid = fromStatus <$> status <*> traffic where
    def = PorticaStatus False False
    status  = readConfigPropertyDef uuid vmPorticaStatus def
    traffic = getVmSeamlessTraffic uuid
    -- ui expects kinky stuff here
    fromStatus s False | not (porticaInstalled s)        = 0
                       | otherwise                       = 2
    fromStatus s True  | not (porticaInstalled s)        = 0
                       | not (Vm.Types.porticaEnabled s) = 2
                       | otherwise                       = 1

getVmPorticaInstalled :: Uuid -> Rpc Bool
getVmPorticaInstalled uuid =
    readConfigPropertyDef uuid vmPorticaStatus (PorticaStatus False False) >>= return . porticaInstalled

getSeamlessVms :: Rpc [Uuid]
getSeamlessVms = filterM getVmSeamlessTraffic =<< getVms

getVmSeamlessTraffic :: Uuid -> Rpc Bool
getVmSeamlessTraffic uuid = of_type =<< getVmType uuid where
    of_type Svm = readConfigProperty uuid vmSeamlessTraffic >>= of_config
    of_type _   = return False

    of_config (Just v) = return v
    of_config Nothing  = appSeamlessTrafficDefault

getVmAutostartPending :: Uuid -> Rpc Bool
getVmAutostartPending uuid = do
    uuids <- dbReadWithDefault [] "/xenmgr/autostart-pending-vms" :: Rpc [Uuid]
    return $ uuid `elem` uuids

getVmHibernated :: Uuid -> Rpc Bool
getVmHibernated uuid = readConfigPropertyDef uuid vmHibernated False

-- returns in MiBs
getVmMemoryStaticMax :: Uuid -> Rpc Int
getVmMemoryStaticMax uuid =
  do v <- readConfigPropertyDef uuid vmMemoryStaticMax 0
     if v <= 0
        then getVmMemory uuid
        else return v

-- in MiBs
getVmMemoryMin :: Uuid -> Rpc Int
getVmMemoryMin uuid =
  do v <- readConfigPropertyDef uuid vmMemoryMin 0
     if v <= 0
        then getVmMemory uuid
        else return v

-- returns in MiBs
getVmMemoryTarget :: Uuid -> Rpc Int
getVmMemoryTarget uuid = fromIntegral . kibToMib <$> getVmMemoryTargetKib uuid

getVmMemoryTargetKib :: Uuid -> Rpc Integer
getVmMemoryTargetKib uuid = whenDomainID 0 uuid kibS where
  kibS :: DomainID -> Rpc Integer
  kibS domid = do v <- liftIO $ xsRead ("/local/domain/" ++ show domid ++ "/memory/target")
                  return $ maybe 0 read v

getVmStartOnBoot :: Uuid -> Rpc Bool
getVmStartOnBoot uuid = nativeOverride uuid True $ readConfigPropertyDef uuid vmStartOnBoot False

getVmHiddenInSwitcher :: Uuid -> Rpc Bool
getVmHiddenInSwitcher uuid = readConfigPropertyDef uuid vmHidden False

getVmHiddenInUi :: Uuid -> Rpc Bool
getVmHiddenInUi uuid = readConfigPropertyDef uuid vmHiddenInUi False

-- returns in MBs
getVmMemory :: Uuid -> Rpc Int
getVmMemory uuid = readConfigPropertyDef uuid vmMemory 0

getVmName :: Uuid -> Rpc String
getVmName uuid = readConfigPropertyDef uuid vmName ""

getVmImagePath :: Uuid -> Rpc FilePath
getVmImagePath uuid = readConfigPropertyDef uuid vmImagePath ""

getVmSlot :: Uuid -> Rpc Int
getVmSlot uuid = readConfigPropertyDef uuid vmSlot 0

getVmPvAddons :: MonadRpc e m => Uuid -> m Bool
getVmPvAddons uuid = readConfigPropertyDef uuid vmPvAddons False

getVmPvAddonsVersion :: Uuid -> Rpc String
getVmPvAddonsVersion uuid = readConfigPropertyDef uuid vmPvAddonsVersion ""

getVmTimeOffset :: Uuid -> Rpc Int
getVmTimeOffset uuid = readConfigPropertyDef uuid vmTimeOffset 0

getVmCryptoUser :: Uuid -> Rpc String
getVmCryptoUser uuid = readConfigPropertyDef uuid vmCryptoUser ""

getVmCryptoKeyDirs :: Uuid -> Rpc String
getVmCryptoKeyDirs uuid = readConfigPropertyDef uuid vmCryptoKeyDirs ""

getVmAutoS3Wake :: Uuid -> Rpc Bool
getVmAutoS3Wake uuid = readConfigPropertyDef uuid vmAutoS3Wake False

getVmNotify uuid = readConfigPropertyDef uuid vmNotify ""
getVmHvm uuid = readConfigPropertyDef uuid vmHvm False
getVmPae uuid = readConfigPropertyDef uuid vmPae False
getVmApic uuid = readConfigPropertyDef uuid vmApic False
getVmAcpi uuid = readConfigPropertyDef uuid vmAcpi False
getVmViridian uuid = readConfigPropertyDef uuid vmViridian False
getVmNx uuid = readConfigPropertyDef uuid vmNx False
getVmSound uuid = readConfigPropertyDef uuid vmSound ""
getVmDisplay uuid = readConfigPropertyDef uuid vmDisplay ""
getVmBoot uuid = readConfigPropertyDef uuid vmBoot ""
getVmCmdLine uuid = readConfigPropertyDef uuid vmCmdLine ""
getVmKernel uuid = readConfigPropertyDef uuid vmKernel ""
getVmKernelExtract uuid = readConfigPropertyDef uuid vmKernelExtract ""
getVmInitrd uuid = readConfigPropertyDef uuid vmInitrd ""
getVmInitrdExtract uuid = readConfigPropertyDef uuid vmInitrdExtract ""
getVmAcpiPath uuid = readConfigPropertyDef uuid vmAcpiPath ""
getVmVcpus uuid = readConfigPropertyDef uuid vmVcpus (0::Int)
getVmCoresPerSocket uuid = readConfigPropertyDef uuid vmCoresPerSocket (0::Int)
getVmVideoram uuid = readConfigPropertyDef uuid vmVideoram (0::Int)
getVmPassthroughMmio uuid = readConfigPropertyDef uuid vmPassthroughMmio ""
getVmPassthroughIo uuid = readConfigPropertyDef uuid vmPassthroughIo ""
getVmFlaskLabel uuid = readConfigPropertyDef uuid vmFlaskLabel ""
getVmHap uuid = readConfigPropertyDef uuid vmHap False
getVmSmbios uuid = readConfigPropertyDef uuid vmSmbios ""
getVmDescription uuid = readConfigPropertyDef uuid vmDescription ""
getVmStartOnBootPriority uuid = readConfigPropertyDef uuid vmStartOnBootPriority (0::Int)
getVmKeepAlive uuid = readConfigPropertyDef uuid vmKeepAlive False
getVmProvidesNetworkBackend uuid = readConfigPropertyDef uuid vmProvidesNetworkBackend False
getVmProvidesDefaultNetworkBackend uuid = readConfigPropertyDef uuid vmProvidesDefaultNetworkBackend False
getVmMeasured uuid = readConfigPropertyDef uuid vmMeasured False
getVmProvidesGraphicsFallback uuid = readConfigPropertyDef uuid vmProvidesGraphicsFallback False
getVmSeamlessId uuid = readConfigPropertyDef uuid vmSeamlessId ""
getVmStartFromSuspendImage uuid = readConfigPropertyDef uuid vmStartFromSuspendImage ""
getVmBedOperation uuid = dbMaybeRead ("/vm/" ++ show uuid ++ "/backend/state/operation")
getVmQemuDmPath uuid = readConfigPropertyDef uuid vmQemuDmPath "/opt/xensource/libexec/qemu-dm-wrapper"
getVmQemuDmTimeout uuid = readConfigPropertyDef uuid vmQemuDmTimeout (30::Int)
getVmGreedyPcibackBind uuid = readConfigPropertyDef uuid vmGreedyPcibackBind True
getVmRunPostCreate uuid = readConfigProperty uuid vmRunPostCreate
getVmRunPreDelete uuid = readConfigProperty uuid vmRunPreDelete
getVmRunPreBoot uuid = readConfigProperty uuid vmRunPreBoot
getVmRunInsteadofStart uuid = readConfigProperty uuid vmRunInsteadofStart
getVmRunOnStateChange uuid = readConfigProperty uuid vmRunOnStateChange
getVmRunOnAcpiStateChange uuid = readConfigProperty uuid vmRunOnAcpiStateChange
getVmS3Mode uuid = readConfigPropertyDef uuid vmS3Mode S3Pv
getVmS4Mode uuid = readConfigPropertyDef uuid vmS4Mode S4Pv
getVmVsnd uuid = readConfigPropertyDef uuid vmVsnd False

getVmShutdownPriority uuid =
    readConfigProperty uuid vmShutdownPriority >>= test where
        test (Just pri) = return pri
        -- default shutdown priority of pvm is lower so it gets shutdown later on
        test Nothing = getVmGraphics uuid >>= \t -> return $ case t of
                                                           HDX ->  (-10)
                                                           _   -> (0 :: Int)

getVmExtraXenvm uuid = concat . intersperse ";" <$> readConfigPropertyDef uuid vmExtraXenvm []
getVmExtraHvm   uuid = concat . intersperse ";" <$> readConfigPropertyDef uuid vmExtraHvms []

-- 'native experience' related properties
nativeOverride uuid value f
  = ifM (getVmNativeExperience uuid) (return value) f

getVmNativeExperience uuid = readConfigPropertyDef uuid vmNativeExperience False
getVmShowSwitcher uuid = nativeOverride uuid False $ readConfigPropertyDef uuid vmShowSwitcher True
getVmWirelessControl uuid = nativeOverride uuid True $ readConfigPropertyDef uuid vmWirelessControl False
getVmUsbGrabDevices uuid = nativeOverride uuid True $ readConfigPropertyDef uuid vmUsbGrabDevices False
getVmControlPlatformPowerState uuid = nativeOverride uuid True $ readConfigPropertyDef uuid vmControlPlatformPowerState False

getVmRealm uuid = readConfigPropertyDef uuid vmRealm ""
getVmSyncUuid uuid = readConfigPropertyDef uuid vmSyncUuid ""
getVmIcbinnPath uuid = readConfigPropertyDef uuid vmIcbinnPath ""
getVmOvfTransportIso uuid = readConfigPropertyDef uuid vmOvfTransportIso False
getVmDownloadProgress uuid = fromMaybe (0::Int) <$> dbRead ("/vm/"++show uuid++"/download-progress")
getVmReady uuid = readConfigPropertyDef uuid vmReady True
getVmVkb uuid = readConfigPropertyDef uuid vmVkb False
getVmVfb uuid = readConfigPropertyDef uuid vmVfb False
getVmV4V uuid = readConfigPropertyDef uuid vmV4v False
getVmRestrictDisplayDepth uuid = readConfigPropertyDef uuid vmRestrictDisplayDepth False
getVmRestrictDisplayRes uuid = readConfigPropertyDef uuid vmRestrictDisplayRes False
getVmPreserveOnReboot uuid = readConfigPropertyDef uuid vmPreserveOnReboot False
getVmBootSentinel uuid = f <$> readConfigPropertyDef uuid vmBootSentinel "" where
  f "" = Nothing
  f x  = Just x
getVmHpet uuid = readConfigPropertyDef uuid vmHpet vmHpetDefault
getVmTimerMode uuid = readConfigPropertyDef uuid vmTimerMode vmTimerModeDefault
getVmNestedHvm uuid = readConfigPropertyDef uuid vmNestedHvm False
getVmSerial uuid = readConfigPropertyDef uuid vmSerial ""
