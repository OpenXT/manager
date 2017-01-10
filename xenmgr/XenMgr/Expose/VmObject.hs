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

module XenMgr.Expose.VmObject (expose) where

import Control.Monad
import Control.Applicative
import Data.String
import Data.Int
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map as M
import System.FilePath.Posix
import Text.Printf

import Tools.File
import Tools.Misc
import Tools.Log
import Tools.Text
import Tools.Apptool
import Tools.IfM

import XenMgr.Rpc
import XenMgr.Db
import Rpc.Autogen.XenmgrVmServer
import Rpc.Autogen.XenmgrConst

import Vm.Types
import Vm.Queries
import Vm.QueriesM
import Vm.Actions
import Vm.Policies
import Vm.Config
import Vm.Pci
import Vm.Monad
import Vm.State
import Vm.ProductProperty

import qualified Vm.V4VFirewall as Firewall
import qualified XenMgr.Connect.Xl as Xl
import XenMgr.Errors
import qualified XenMgr.Expose.VmDiskObject as VmDiskObj
import qualified XenMgr.Expose.VmNicObject as VmNicObj
import Data.String
import XenMgr.Expose.ObjectPaths
import Vm.Templates
import XenMgr.Notify
import XenMgr.XM
import Rpc.Core

-- Put single VM on dbus
expose :: Uuid -> XM ()
expose uuid =
    do xm <- xmContext
       liftRpc $ rpcExpose (vmObjPath uuid) (interfaces $ implementationFor xm uuid)
       info $ "exposed VM " ++ show uuid ++ " over DBUS"

       disk_ids <- sort . M.keys <$> liftRpc (getDisks uuid)
       mapM_ (VmDiskObj.expose uuid) disk_ids

       nic_ids <- sort <$> liftRpc (getNicIds uuid)
       mapM_ (VmNicObj.expose uuid) nic_ids

-- just some utility used below to convert function working on Uuids into function working on string
-- (as uuids get passed as strings through rpc calls)

uuidize :: (Uuid -> a) -> (String -> a)
uuidize f = f . fromString

implementationFor xm uuid = self where
  runxm = runXM xm
  runvm = runXM xm . xmRunVm uuid
  self = XenmgrVmServer {
    comCitrixXenclientXenmgrVmAuthAuthRequired   = _auth_required uuid
  , comCitrixXenclientXenmgrVmAuthAuth           = _auth uuid
  , comCitrixXenclientXenmgrVmPciAddPtRule       = _add_pt_rule uuid
  , comCitrixXenclientXenmgrVmPciAddPtRuleBdf    = _add_pt_rule_bdf uuid
  , comCitrixXenclientXenmgrVmPciDeletePtRule    = _delete_pt_rule uuid
  , comCitrixXenclientXenmgrVmPciDeletePtRuleBdf = _delete_pt_rule_bdf uuid
  , comCitrixXenclientXenmgrVmPciListPtRules     = _list_pt_rules uuid
  , comCitrixXenclientXenmgrVmPciListPtPciDevices= _list_pt_pci_devices uuid
  , comCitrixXenclientXenmgrVmListDisks          = _list_disks uuid
  , comCitrixXenclientXenmgrVmListNics           = _list_nics uuid
  , comCitrixXenclientXenmgrVmAddDisk            = runxm $ _add_disk uuid
  , comCitrixXenclientXenmgrVmAddNic             = runxm $ _add_nic uuid

  , comCitrixXenclientXenmgrVmDelete             = unlessM policyQueryVmDeletion failActionSuppressedByPolicy >> removeVm uuid
  , comCitrixXenclientXenmgrVmSwitch             = switchVm uuid >> return ()
  , comCitrixXenclientXenmgrVmStart              = runXM xm (startVm uuid) >> return ()
  , comCitrixXenclientXenmgrVmStartInternal      = runXM xm (startVmInternal uuid) >> return ()
  , comCitrixXenclientXenmgrVmReboot             = rebootVm uuid
  , comCitrixXenclientXenmgrVmShutdown           = runvm invokeShutdownVm
  , comCitrixXenclientXenmgrVmDestroy            = runvm invokeForceShutdownVm
  , comCitrixXenclientXenmgrVmSleep              = sleepVm uuid
  , comCitrixXenclientXenmgrVmHibernate          = hibernateVm uuid
  , comCitrixXenclientXenmgrVmResume             = liftIO $ Xl.resumeFromSleep uuid >> return ()
  , comCitrixXenclientXenmgrVmSuspendToFile      = \f -> liftIO $ suspendToFile uuid f
  , comCitrixXenclientXenmgrVmResumeFromFile     = \f -> liftIO $ resumeFromFile uuid f False False
  , comCitrixXenclientXenmgrVmPause              = pauseVm uuid
  , comCitrixXenclientXenmgrVmUnpause            = unpauseVm uuid
  , comCitrixXenclientXenmgrVmReadIcon           = getVmIconBytes uuid
  , comCitrixXenclientXenmgrVmGetDbKey           = _get_db_key uuid
  , comCitrixXenclientXenmgrVmSetDbKey           = _set_db_key uuid
  , comCitrixXenclientXenmgrVmGetDomstoreKey     = _get_domstore_key uuid
  , comCitrixXenclientXenmgrVmSetDomstoreKey     = _set_domstore_key uuid
  , comCitrixXenclientXenmgrVmCreateChildServiceVm = runXM xm . _create_child_service_vm uuid
  , comCitrixXenclientXenmgrVmListV4vFirewallRules = _list_v4v_firewall_rules uuid
  , comCitrixXenclientXenmgrVmAddV4vFirewallRule = _add_v4v_firewall_rule uuid
  , comCitrixXenclientXenmgrVmDeleteV4vFirewallRule = _delete_v4v_firewall_rule uuid

  , comCitrixXenclientXenmgrVmListNetFirewallRules = _list_firewall_rules uuid
  , comCitrixXenclientXenmgrVmAddNetFirewallRule = _add_firewall_rule uuid
  , comCitrixXenclientXenmgrVmDeleteNetFirewallRule = _delete_firewall_rule uuid
  -- bucketload of properties - unrestricted version
  ---------------------------
  , comCitrixXenclientXenmgrVmUnrestrictedGetState = runvm _state_str
  , comCitrixXenclientXenmgrVmUnrestrictedGetAcpiState = fromIntegral <$> getVmAcpiState uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetDomid = fromIntegral . fromMaybe (-1) <$> getDomainID uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetType = _type_to_str <$> getVmType uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetType = \t -> setVmType uuid (_type_of_str t)
  , comCitrixXenclientXenmgrVmUnrestrictedGetName = getVmName uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetName = setVmName uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetDescription = getVmDescription uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetDescription = \v -> setVmDescription uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetUuid = return $ show uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetSlot = fromIntegral <$> getVmSlot uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetSlot = \s -> runXM xm $ setVmSlot uuid (fromIntegral s)
  , comCitrixXenclientXenmgrVmUnrestrictedGetPvAddons = getVmPvAddons uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPvAddons = setVmPvAddons uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPvAddonsVersion = getVmPvAddonsVersion uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPvAddonsVersion = setVmPvAddonsVersion uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetStartOnBoot = getVmStartOnBoot uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetStartOnBoot = setVmStartOnBoot uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetTimeOffset = fromIntegral <$> getVmTimeOffset uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetTimeOffset = \o -> setVmTimeOffset uuid (fromIntegral o)
  , comCitrixXenclientXenmgrVmUnrestrictedGetCryptoUser = getVmCryptoUser uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetCryptoUser = setVmCryptoUser uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetCryptoKeyDirs = getVmCryptoKeyDirs uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetCryptoKeyDirs = \v -> setVmCryptoKeyDirs uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetAutoS3Wake = getVmAutoS3Wake uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetAutoS3Wake = setVmAutoS3Wake uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetImagePath = getVmImagePath uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetImagePath = setVmImagePath uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetWiredNetwork = fromMaybe "" . fmap networkToStr <$> getVmWiredNetwork uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetWiredNetwork = \v -> runxm $ setVmWiredNetwork uuid (networkFromStr v)
  , comCitrixXenclientXenmgrVmUnrestrictedGetWirelessNetwork = fromMaybe "" . fmap networkToStr <$> getVmWirelessNetwork uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetWirelessNetwork = \v -> runxm $ setVmWirelessNetwork uuid (networkFromStr v)
  , comCitrixXenclientXenmgrVmUnrestrictedGetGpu = getVmGpu uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetGpu = setVmGpu uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetCd = getVmCd uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetCd = setVmCd uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetMac = getVmMac uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetAmtPt = getVmAmtPt uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetAmtPt = setVmAmtPt uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPorticaEnabled = fromIntegral <$> getVmPorticaEnabled uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPorticaInstalled = getVmPorticaInstalled uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetSeamlessTraffic = getVmSeamlessTraffic uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetSeamlessTraffic = setVmSeamlessTraffic uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetAutostartPending = getVmAutostartPending uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetHibernated = getVmHibernated uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetMemoryStaticMax = fromIntegral <$> getVmMemoryStaticMax uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetMemoryStaticMax = \m -> setVmMemoryStaticMax uuid (fromIntegral m)
  , comCitrixXenclientXenmgrVmUnrestrictedGetMemoryTarget = fromIntegral <$> getVmMemoryTarget uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetMemory = fromIntegral <$> getVmMemory uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetMemory = \m -> setVmMemory uuid (fromIntegral m)
  , comCitrixXenclientXenmgrVmUnrestrictedGetMemoryMin = fromIntegral <$> getVmMemoryMin uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetMemoryMin = \m -> setVmMemoryMin uuid (fromIntegral m)

  , comCitrixXenclientXenmgrVmUnrestrictedGetHiddenInSwitcher = getVmHiddenInSwitcher uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetHiddenInSwitcher = setVmHiddenInSwitcher uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetHiddenInUi = getVmHiddenInUi uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetHiddenInUi = setVmHiddenInUi uuid

  , comCitrixXenclientXenmgrVmUnrestrictedGetNotify = getVmNotify uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetNotify = setVmNotify uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetHvm = getVmHvm uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetHvm = \v -> setVmHvm uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetPae = getVmPae uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPae = \v -> setVmPae uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetApic = getVmApic uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetApic = \v -> setVmApic uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetAcpi = getVmAcpi uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetAcpi = \v -> setVmAcpi uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetViridian = getVmViridian uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetViridian = \v -> setVmViridian uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetNx = getVmNx uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetNx = \v -> setVmNx uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetHap = getVmHap uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetHap = \v -> setVmHap uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetSmbios = getVmSmbios uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetSmbios = \v -> setVmSmbios uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetSound = getVmSound uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetSound = setVmSound uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetDisplay = getVmDisplay uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetDisplay = setVmDisplay uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetBoot = getVmBoot uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetBoot = setVmBoot uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetCmdLine = getVmCmdLine uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetCmdLine = setVmCmdLine uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetKernel = getVmKernel uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetKernel = setVmKernel uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetKernelExtract = getVmKernelExtract uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetKernelExtract = setVmKernelExtract uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetInitrd = getVmInitrd uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetInitrd = setVmInitrd uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetAcpiPath = getVmAcpiPath uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetAcpiPath = \v -> setVmAcpiPath uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetVcpus  = fromIntegral <$> getVmVcpus uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetVcpus  = \v -> setVmVcpus uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmUnrestrictedGetCoresPerSocket = fromIntegral <$> getVmCoresPerSocket uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetCoresPerSocket = \v -> setVmCoresPerSocket uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmUnrestrictedGetVideoram = fromIntegral <$> getVmVideoram uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetVideoram = \v -> setVmVideoram uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmUnrestrictedGetPassthroughMmio = getVmPassthroughMmio uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPassthroughMmio = setVmPassthroughMmio uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPassthroughIo = getVmPassthroughIo uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPassthroughIo = setVmPassthroughIo uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetFlaskLabel = getVmFlaskLabel uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetFlaskLabel = setVmFlaskLabel uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetStartOnBootPriority = fromIntegral <$> getVmStartOnBootPriority uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetStartOnBootPriority = \v -> setVmStartOnBootPriority uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmUnrestrictedGetKeepAlive = getVmKeepAlive uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetKeepAlive = setVmKeepAlive uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetProvidesNetworkBackend = getVmProvidesNetworkBackend uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetProvidesNetworkBackend = setVmProvidesNetworkBackend uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetMeasured = getVmMeasured uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetProvidesGraphicsFallback = getVmProvidesGraphicsFallback uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetProvidesGraphicsFallback = \v -> setVmProvidesGraphicsFallback uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetShutdownPriority = fromIntegral <$> getVmShutdownPriority uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetShutdownPriority = \v -> setVmShutdownPriority uuid (fromIntegral v)

  , comCitrixXenclientXenmgrVmUnrestrictedGetExtraXenvm = getVmExtraXenvm uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetExtraXenvm = \v -> setVmExtraXenvm uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetExtraHvm = getVmExtraHvm uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetExtraHvm = \v -> setVmExtraHvm uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetSeamlessId = getVmSeamlessId uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetSeamlessId = \v -> setVmSeamlessId uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetStartFromSuspendImage = getVmStartFromSuspendImage uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetStartFromSuspendImage = \v -> setVmStartFromSuspendImage uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetQemuDmPath = getVmQemuDmPath uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetQemuDmPath = \v -> setVmQemuDmPath uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetQemuDmTimeout = fromIntegral <$> getVmQemuDmTimeout uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetQemuDmTimeout = \v -> setVmQemuDmTimeout uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmUnrestrictedGetTrackDependencies = getVmTrackDependencies uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetTrackDependencies = \v -> setVmTrackDependencies uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetDependencies = getVmDependencies uuid >>= return . map vmObjPath

  , comCitrixXenclientXenmgrVmUnrestrictedGetSeamlessMouseLeft = fromIntegral <$> getVmSeamlessMouseLeft uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetSeamlessMouseRight = fromIntegral <$> getVmSeamlessMouseRight uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetOs = osToStr <$> getVmOs uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetOs = \v -> case osFromStr v of Just os -> setVmOs uuid os
                                                                            _ -> error "unknown os"
  , comCitrixXenclientXenmgrVmUnrestrictedGetOemAcpiFeatures = getVmOemAcpiFeatures uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetOemAcpiFeatures = \v -> setVmOemAcpiFeatures uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetControlPlatformPowerState = getVmControlPlatformPowerState uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetControlPlatformPowerState = \v -> setVmControlPlatformPowerState uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetUsbControl = getVmUsbControl uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetUsbControl = \v -> setVmUsbControl uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetUsbEnabled = getVmUsbEnabled uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetUsbEnabled = \v -> setVmUsbEnabled uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetUsbAutoPassthrough = getVmUsbAutoPassthrough uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetUsbAutoPassthrough = \v -> setVmUsbAutoPassthrough uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetUsbGrabDevices = getVmUsbGrabDevices uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetUsbGrabDevices = \v -> setVmUsbGrabDevices uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetStubdom = getVmStubdom uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetStubdom = \v -> setVmStubdom uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetStubdomMemory = fromIntegral <$> getVmStubdomMemory uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetStubdomMemory = \v -> setVmStubdomMemory uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmUnrestrictedGetStubdomCmdline = getVmStubdomCmdline uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetStubdomCmdline = \v -> setVmStubdomCmdline uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetCpuid = getVmCpuid uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetCpuid = \v -> setVmCpuid uuid v
  , comCitrixXenclientXenmgrVmUnrestrictedGetGreedyPcibackBind = getVmGreedyPcibackBind uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetGreedyPcibackBind = \v -> setVmGreedyPcibackBind uuid v

  , comCitrixXenclientXenmgrVmUnrestrictedGetPolicyModifyVmSettings = policyQueryModifyVmSettings uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPolicyCdAccess = policyQueryCdAccess uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPolicyCdRecording = policyQueryCdRecording uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPolicyAudioAccess = policyQueryAudioAccess uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPolicyAudioRecording = policyQueryAudioRecording uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPolicyWiredNetworking = policyQueryWiredNetworking uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPolicyWirelessNetworking = policyQueryWifiNetworking uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPolicyPrintScreen = policyQueryPrintScreen uuid

  , comCitrixXenclientXenmgrVmUnrestrictedSetPolicyModifyVmSettings = policySetModifyVmSettings uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPolicyCdAccess = policySetCdAccess uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPolicyCdRecording = policySetCdRecording uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPolicyAudioAccess = policySetAudioAccess uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPolicyAudioRecording = policySetAudioRecording uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPolicyWiredNetworking = policySetWiredNetworking uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPolicyWirelessNetworking = policySetWifiNetworking uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPolicyPrintScreen = policySetPrintScreen uuid

  , comCitrixXenclientXenmgrVmUnrestrictedGetRunPostCreate = getruncmd getVmRunPostCreate
  , comCitrixXenclientXenmgrVmUnrestrictedGetRunPreDelete = getruncmd getVmRunPreDelete
  , comCitrixXenclientXenmgrVmUnrestrictedGetRunPreBoot = getruncmd getVmRunPreBoot
  , comCitrixXenclientXenmgrVmUnrestrictedGetRunInsteadofStart = getruncmd getVmRunInsteadofStart
  , comCitrixXenclientXenmgrVmUnrestrictedGetRunOnStateChange = getruncmd getVmRunOnStateChange
  , comCitrixXenclientXenmgrVmUnrestrictedGetRunOnAcpiStateChange = getruncmd getVmRunOnAcpiStateChange

  , comCitrixXenclientXenmgrVmUnrestrictedSetRunPostCreate = setruncmd setVmRunPostCreate
  , comCitrixXenclientXenmgrVmUnrestrictedSetRunPreDelete = setruncmd setVmRunPreDelete
  , comCitrixXenclientXenmgrVmUnrestrictedSetRunPreBoot = setruncmd setVmRunPreBoot
  , comCitrixXenclientXenmgrVmUnrestrictedSetRunInsteadofStart = setruncmd setVmRunInsteadofStart
  , comCitrixXenclientXenmgrVmUnrestrictedSetRunOnStateChange = setruncmd setVmRunOnStateChange
  , comCitrixXenclientXenmgrVmUnrestrictedSetRunOnAcpiStateChange = setruncmd setVmRunOnAcpiStateChange

  , comCitrixXenclientXenmgrVmUnrestrictedSetDomstoreReadAccess = setDomstoreReadAccess uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetDomstoreReadAccess = getDomstoreReadAccess uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetDomstoreWriteAccess = setDomstoreWriteAccess uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetDomstoreWriteAccess = getDomstoreWriteAccess uuid

    -- bucketload of properties -- restricted version
    -------------------------------------------------
  , comCitrixXenclientXenmgrVmGetState = runvm _state_str
  , comCitrixXenclientXenmgrVmGetAcpiState = fromIntegral <$> getVmAcpiState uuid
  , comCitrixXenclientXenmgrVmGetDomid = fromIntegral . fromMaybe (-1) <$> getDomainID uuid
  , comCitrixXenclientXenmgrVmGetType = _type_to_str <$> getVmType uuid
  , comCitrixXenclientXenmgrVmSetType = \t -> restrict >> setVmType uuid (_type_of_str t)
  , comCitrixXenclientXenmgrVmGetName = getVmName uuid
  , comCitrixXenclientXenmgrVmSetName = \v -> restrict >> setVmName uuid v
  , comCitrixXenclientXenmgrVmGetDescription = getVmDescription uuid
  , comCitrixXenclientXenmgrVmSetDescription = \v -> restrict >> setVmDescription uuid v
  , comCitrixXenclientXenmgrVmGetUuid = return $ show uuid
  , comCitrixXenclientXenmgrVmGetSlot = fromIntegral <$> getVmSlot uuid
  , comCitrixXenclientXenmgrVmSetSlot = \s -> restrict >> runXM xm (setVmSlot uuid (fromIntegral s))
  , comCitrixXenclientXenmgrVmGetPvAddons = getVmPvAddons uuid
  , comCitrixXenclientXenmgrVmSetPvAddons = \v -> restrict >> setVmPvAddons uuid v
  , comCitrixXenclientXenmgrVmGetPvAddonsVersion = getVmPvAddonsVersion uuid
  , comCitrixXenclientXenmgrVmSetPvAddonsVersion = \v -> restrict >> setVmPvAddonsVersion uuid v
  , comCitrixXenclientXenmgrVmGetStartOnBoot = getVmStartOnBoot uuid
  , comCitrixXenclientXenmgrVmSetStartOnBoot = \v -> restrict >> setVmStartOnBoot uuid v
  , comCitrixXenclientXenmgrVmGetTimeOffset = fromIntegral <$> getVmTimeOffset uuid
  , comCitrixXenclientXenmgrVmSetTimeOffset = \o -> restrict >> setVmTimeOffset uuid (fromIntegral o)
  , comCitrixXenclientXenmgrVmGetCryptoUser = getVmCryptoUser uuid
  , comCitrixXenclientXenmgrVmSetCryptoUser = \v -> restrict >> setVmCryptoUser uuid v
  , comCitrixXenclientXenmgrVmGetCryptoKeyDirs = getVmCryptoKeyDirs uuid
  , comCitrixXenclientXenmgrVmSetCryptoKeyDirs = \v -> restrict >> setVmCryptoKeyDirs uuid v
  , comCitrixXenclientXenmgrVmGetAutoS3Wake = getVmAutoS3Wake uuid
  , comCitrixXenclientXenmgrVmSetAutoS3Wake = \v -> restrict >> setVmAutoS3Wake uuid v
  , comCitrixXenclientXenmgrVmGetImagePath = getVmImagePath uuid
  , comCitrixXenclientXenmgrVmSetImagePath = \v -> restrict >> setVmImagePath uuid v
  , comCitrixXenclientXenmgrVmGetWiredNetwork = fromMaybe "" . fmap networkToStr <$> getVmWiredNetwork uuid
  , comCitrixXenclientXenmgrVmSetWiredNetwork = \v -> runxm $ setVmWiredNetwork uuid (networkFromStr v)
  , comCitrixXenclientXenmgrVmGetWirelessNetwork = fromMaybe "" . fmap networkToStr <$> getVmWirelessNetwork uuid
  , comCitrixXenclientXenmgrVmSetWirelessNetwork = \v -> runxm $ setVmWirelessNetwork uuid (networkFromStr v)
  , comCitrixXenclientXenmgrVmGetGpu = getVmGpu uuid
  , comCitrixXenclientXenmgrVmSetGpu = \v -> restrict >> setVmGpu uuid v
  , comCitrixXenclientXenmgrVmGetCd = getVmCd uuid
  , comCitrixXenclientXenmgrVmSetCd = \v -> restrict >> setVmCd uuid v
  , comCitrixXenclientXenmgrVmGetMac = getVmMac uuid
  , comCitrixXenclientXenmgrVmGetAmtPt = getVmAmtPt uuid
  , comCitrixXenclientXenmgrVmSetAmtPt = \v -> restrict >> setVmAmtPt uuid v
  , comCitrixXenclientXenmgrVmGetPorticaEnabled = fromIntegral <$> getVmPorticaEnabled uuid
  , comCitrixXenclientXenmgrVmGetPorticaInstalled = getVmPorticaInstalled uuid
  , comCitrixXenclientXenmgrVmGetSeamlessTraffic = getVmSeamlessTraffic uuid
  , comCitrixXenclientXenmgrVmSetSeamlessTraffic = \v -> restrict >> setVmSeamlessTraffic uuid v
  , comCitrixXenclientXenmgrVmGetAutostartPending = getVmAutostartPending uuid
  , comCitrixXenclientXenmgrVmGetHibernated = getVmHibernated uuid
  , comCitrixXenclientXenmgrVmGetMemoryStaticMax = fromIntegral <$> getVmMemoryStaticMax uuid
  , comCitrixXenclientXenmgrVmSetMemoryStaticMax = \m -> restrict >> setVmMemoryStaticMax uuid (fromIntegral m)
  , comCitrixXenclientXenmgrVmGetMemoryTarget = fromIntegral <$> getVmMemoryTarget uuid
  , comCitrixXenclientXenmgrVmGetMemory = fromIntegral <$> getVmMemory uuid
  , comCitrixXenclientXenmgrVmSetMemory = \m -> restrict >> setVmMemory uuid (fromIntegral m)
  , comCitrixXenclientXenmgrVmGetMemoryMin = fromIntegral <$> getVmMemoryMin uuid
  , comCitrixXenclientXenmgrVmSetMemoryMin = \m -> restrict >> setVmMemoryMin uuid (fromIntegral m)
  , comCitrixXenclientXenmgrVmGetHiddenInSwitcher = getVmHiddenInSwitcher uuid
  , comCitrixXenclientXenmgrVmSetHiddenInSwitcher = \v -> restrict >> setVmHiddenInSwitcher uuid v
  , comCitrixXenclientXenmgrVmGetHiddenInUi = getVmHiddenInUi uuid
  , comCitrixXenclientXenmgrVmSetHiddenInUi = \v -> restrict >> setVmHiddenInUi uuid v
  , comCitrixXenclientXenmgrVmGetMeasured = getVmMeasured uuid
  , comCitrixXenclientXenmgrVmGetProvidesGraphicsFallback = getVmProvidesGraphicsFallback uuid
  , comCitrixXenclientXenmgrVmSetProvidesGraphicsFallback = \v -> restrict >> setVmProvidesGraphicsFallback uuid v
  , comCitrixXenclientXenmgrVmGetShutdownPriority = fromIntegral <$> getVmShutdownPriority uuid
  , comCitrixXenclientXenmgrVmSetShutdownPriority = \v -> restrict >> setVmShutdownPriority uuid (fromIntegral v)

  , comCitrixXenclientXenmgrVmGetNotify = getVmNotify uuid
  , comCitrixXenclientXenmgrVmSetNotify = \v -> restrict >> setVmNotify uuid v
  , comCitrixXenclientXenmgrVmGetHvm = getVmHvm uuid
  , comCitrixXenclientXenmgrVmSetHvm = \v -> restrict >> setVmHvm uuid v
  , comCitrixXenclientXenmgrVmGetPae = getVmPae uuid
  , comCitrixXenclientXenmgrVmSetPae = \v -> restrict >> setVmPae uuid v
  , comCitrixXenclientXenmgrVmGetApic = getVmApic uuid
  , comCitrixXenclientXenmgrVmSetApic = \v -> restrict >> setVmApic uuid v
  , comCitrixXenclientXenmgrVmGetAcpi = getVmAcpi uuid
  , comCitrixXenclientXenmgrVmSetAcpi = \v -> restrict >> setVmAcpi uuid v
  , comCitrixXenclientXenmgrVmGetViridian = getVmViridian uuid
  , comCitrixXenclientXenmgrVmSetViridian = \v -> restrict >> setVmViridian uuid v
  , comCitrixXenclientXenmgrVmGetNx = getVmNx uuid
  , comCitrixXenclientXenmgrVmSetNx = \v -> restrict >> setVmNx uuid v
  , comCitrixXenclientXenmgrVmGetHap = getVmHap uuid
  , comCitrixXenclientXenmgrVmSetHap = \v -> restrict >> setVmHap uuid v
  , comCitrixXenclientXenmgrVmGetSmbios = getVmSmbios uuid
  , comCitrixXenclientXenmgrVmSetSmbios = \v -> restrict >> setVmSmbios uuid v
  , comCitrixXenclientXenmgrVmGetSound = getVmSound uuid
  , comCitrixXenclientXenmgrVmSetSound = \v -> restrict >> setVmSound uuid v
  , comCitrixXenclientXenmgrVmGetDisplay = getVmDisplay uuid
  , comCitrixXenclientXenmgrVmSetDisplay = \v -> restrict >> setVmDisplay uuid v
  , comCitrixXenclientXenmgrVmGetBoot = getVmBoot uuid
  , comCitrixXenclientXenmgrVmSetBoot = \v -> restrict >> setVmBoot uuid v
  , comCitrixXenclientXenmgrVmGetCmdLine = getVmCmdLine uuid
  , comCitrixXenclientXenmgrVmSetCmdLine = \v -> restrict >> setVmCmdLine uuid v
  , comCitrixXenclientXenmgrVmGetKernel = getVmKernel uuid
  , comCitrixXenclientXenmgrVmSetKernel = \v -> restrict >> setVmKernel uuid v
  , comCitrixXenclientXenmgrVmGetKernelExtract = getVmKernelExtract uuid
  , comCitrixXenclientXenmgrVmSetKernelExtract = \v -> restrict >> setVmKernelExtract uuid v
  , comCitrixXenclientXenmgrVmGetInitrd = getVmInitrd uuid
  , comCitrixXenclientXenmgrVmSetInitrd = \v -> restrict >> setVmInitrd uuid v
  , comCitrixXenclientXenmgrVmGetAcpiPath = getVmAcpiPath uuid
  , comCitrixXenclientXenmgrVmSetAcpiPath = \v -> restrict >> setVmAcpiPath uuid v
  , comCitrixXenclientXenmgrVmGetVcpus  = fromIntegral <$> getVmVcpus uuid
  , comCitrixXenclientXenmgrVmSetVcpus  = \v -> restrict >> setVmVcpus uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmGetCoresPerSocket = fromIntegral <$> getVmCoresPerSocket uuid
  , comCitrixXenclientXenmgrVmSetCoresPerSocket = \v -> restrict >> setVmCoresPerSocket uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmGetVideoram = fromIntegral <$> getVmVideoram uuid
  , comCitrixXenclientXenmgrVmSetVideoram = \v -> restrict >> setVmVideoram uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmGetPassthroughMmio = getVmPassthroughMmio uuid
  , comCitrixXenclientXenmgrVmSetPassthroughMmio = \v -> restrict >> setVmPassthroughMmio uuid v
  , comCitrixXenclientXenmgrVmGetPassthroughIo = getVmPassthroughIo uuid
  , comCitrixXenclientXenmgrVmSetPassthroughIo = \v -> restrict >> setVmPassthroughIo uuid v
  , comCitrixXenclientXenmgrVmGetFlaskLabel = getVmFlaskLabel uuid
  , comCitrixXenclientXenmgrVmSetFlaskLabel = \v -> restrict >> setVmFlaskLabel uuid v
  , comCitrixXenclientXenmgrVmGetStartOnBootPriority = fromIntegral <$> getVmStartOnBootPriority uuid
  , comCitrixXenclientXenmgrVmSetStartOnBootPriority = \v -> restrict >> setVmStartOnBootPriority uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmGetKeepAlive = getVmKeepAlive uuid
  , comCitrixXenclientXenmgrVmSetKeepAlive = \v -> restrict >> setVmKeepAlive uuid v
  , comCitrixXenclientXenmgrVmGetProvidesNetworkBackend = getVmProvidesNetworkBackend uuid
  , comCitrixXenclientXenmgrVmSetProvidesNetworkBackend = \v -> restrict >> setVmProvidesNetworkBackend uuid v
  , comCitrixXenclientXenmgrVmGetExtraXenvm = getVmExtraXenvm uuid
  , comCitrixXenclientXenmgrVmSetExtraXenvm = \v -> restrict >> setVmExtraXenvm uuid v
  , comCitrixXenclientXenmgrVmGetExtraHvm = getVmExtraHvm uuid
  , comCitrixXenclientXenmgrVmSetExtraHvm = \v -> restrict >> setVmExtraHvm uuid v
  , comCitrixXenclientXenmgrVmGetSeamlessId = getVmSeamlessId uuid
  , comCitrixXenclientXenmgrVmSetSeamlessId = \v -> restrict >> setVmSeamlessId uuid v
  , comCitrixXenclientXenmgrVmGetStartFromSuspendImage = getVmStartFromSuspendImage uuid
  , comCitrixXenclientXenmgrVmSetStartFromSuspendImage = \v -> restrict >> setVmStartFromSuspendImage uuid v
  , comCitrixXenclientXenmgrVmGetQemuDmPath = getVmQemuDmPath uuid
  , comCitrixXenclientXenmgrVmSetQemuDmPath = \v -> restrict >> setVmQemuDmPath uuid v
  , comCitrixXenclientXenmgrVmGetQemuDmTimeout = fromIntegral<$> getVmQemuDmTimeout uuid
  , comCitrixXenclientXenmgrVmSetQemuDmTimeout = \v -> restrict >> setVmQemuDmTimeout uuid (fromIntegral v)
  , comCitrixXenclientXenmgrVmGetTrackDependencies = getVmTrackDependencies uuid
  , comCitrixXenclientXenmgrVmSetTrackDependencies = \v -> restrict >> setVmTrackDependencies uuid v
  , comCitrixXenclientXenmgrVmGetDependencies = getVmDependencies uuid >>= return . map vmObjPath

  , comCitrixXenclientXenmgrVmGetSeamlessMouseLeft = fromIntegral <$> getVmSeamlessMouseLeft uuid
  , comCitrixXenclientXenmgrVmGetSeamlessMouseRight = fromIntegral <$> getVmSeamlessMouseRight uuid
  , comCitrixXenclientXenmgrVmGetOs = osToStr <$> getVmOs uuid
  , comCitrixXenclientXenmgrVmSetOs = \v -> restrict >> case osFromStr v of Just os -> setVmOs uuid os
                                                                            _ -> error "unknown os"
  , comCitrixXenclientXenmgrVmGetOemAcpiFeatures = getVmOemAcpiFeatures uuid
  , comCitrixXenclientXenmgrVmSetOemAcpiFeatures = \v -> restrict >> setVmOemAcpiFeatures uuid v
  , comCitrixXenclientXenmgrVmGetControlPlatformPowerState = getVmControlPlatformPowerState uuid
  , comCitrixXenclientXenmgrVmSetControlPlatformPowerState = \v -> restrict >> setVmControlPlatformPowerState uuid v
  , comCitrixXenclientXenmgrVmGetUsbControl = getVmUsbControl uuid
  , comCitrixXenclientXenmgrVmSetUsbControl = \v -> restrict >> setVmUsbControl uuid v
  , comCitrixXenclientXenmgrVmGetUsbEnabled = getVmUsbEnabled uuid
  , comCitrixXenclientXenmgrVmSetUsbEnabled = \v -> restrict >> setVmUsbEnabled uuid v
  , comCitrixXenclientXenmgrVmGetUsbAutoPassthrough = getVmUsbAutoPassthrough uuid
  , comCitrixXenclientXenmgrVmSetUsbAutoPassthrough = \v -> restrict >> setVmUsbAutoPassthrough uuid v
  , comCitrixXenclientXenmgrVmGetUsbGrabDevices = getVmUsbGrabDevices uuid
  , comCitrixXenclientXenmgrVmSetUsbGrabDevices = \v -> restrict >> setVmUsbGrabDevices uuid v
  , comCitrixXenclientXenmgrVmGetStubdom = getVmStubdom uuid
  , comCitrixXenclientXenmgrVmSetStubdom = \v -> restrict >> setVmStubdom uuid v
  , comCitrixXenclientXenmgrVmGetCpuid = getVmCpuid uuid
  , comCitrixXenclientXenmgrVmSetCpuid = \v -> restrict >> setVmCpuid uuid v
  , comCitrixXenclientXenmgrVmGetGreedyPcibackBind = getVmGreedyPcibackBind uuid
  , comCitrixXenclientXenmgrVmSetGreedyPcibackBind = \v -> restrict >> setVmGreedyPcibackBind uuid v

  , comCitrixXenclientXenmgrVmGetPolicyModifyVmSettings = policyQueryModifyVmSettings uuid
  , comCitrixXenclientXenmgrVmGetPolicyCdAccess = policyQueryCdAccess uuid
  , comCitrixXenclientXenmgrVmGetPolicyCdRecording = policyQueryCdRecording uuid
  , comCitrixXenclientXenmgrVmGetPolicyAudioAccess = policyQueryAudioAccess uuid
  , comCitrixXenclientXenmgrVmGetPolicyAudioRecording = policyQueryAudioRecording uuid
  , comCitrixXenclientXenmgrVmGetPolicyWiredNetworking = policyQueryWiredNetworking uuid
  , comCitrixXenclientXenmgrVmGetPolicyWirelessNetworking = policyQueryWifiNetworking uuid
  , comCitrixXenclientXenmgrVmGetPolicyPrintScreen = policyQueryPrintScreen uuid

  , comCitrixXenclientXenmgrVmSetPolicyModifyVmSettings = \v -> restrict >> policySetModifyVmSettings uuid v
  , comCitrixXenclientXenmgrVmSetPolicyCdAccess = \v -> restrict >> policySetCdAccess uuid v
  , comCitrixXenclientXenmgrVmSetPolicyCdRecording = \v -> restrict >> policySetCdRecording uuid v
  , comCitrixXenclientXenmgrVmSetPolicyAudioAccess = \v -> restrict >> policySetAudioAccess uuid v
  , comCitrixXenclientXenmgrVmSetPolicyAudioRecording = \v -> restrict >> policySetAudioRecording uuid v
  , comCitrixXenclientXenmgrVmSetPolicyWiredNetworking = \v -> restrict >> policySetWiredNetworking uuid v
  , comCitrixXenclientXenmgrVmSetPolicyWirelessNetworking = \v -> restrict >> policySetWifiNetworking uuid v
  , comCitrixXenclientXenmgrVmSetPolicyPrintScreen = \v -> restrict >> policySetPrintScreen uuid v

  , comCitrixXenclientXenmgrVmGetRunPostCreate = getruncmd getVmRunPostCreate
  , comCitrixXenclientXenmgrVmGetRunPreDelete = getruncmd getVmRunPreDelete
  , comCitrixXenclientXenmgrVmGetRunPreBoot = getruncmd getVmRunPreBoot
  , comCitrixXenclientXenmgrVmGetRunInsteadofStart = getruncmd getVmRunInsteadofStart
  , comCitrixXenclientXenmgrVmGetRunOnStateChange = getruncmd getVmRunOnStateChange
  , comCitrixXenclientXenmgrVmGetRunOnAcpiStateChange = getruncmd getVmRunOnAcpiStateChange

  , comCitrixXenclientXenmgrVmSetRunPostCreate = \v -> restrict >> setruncmd setVmRunPostCreate v
  , comCitrixXenclientXenmgrVmSetRunPreDelete = \v -> restrict >> setruncmd setVmRunPreDelete v
  , comCitrixXenclientXenmgrVmSetRunPreBoot = \v -> restrict >> setruncmd setVmRunPreBoot v
  , comCitrixXenclientXenmgrVmSetRunInsteadofStart = \v -> restrict >> setruncmd setVmRunInsteadofStart v
  , comCitrixXenclientXenmgrVmSetRunOnStateChange = \v -> restrict >> setruncmd setVmRunOnStateChange v
  , comCitrixXenclientXenmgrVmSetRunOnAcpiStateChange = \v -> restrict >> setruncmd setVmRunOnAcpiStateChange v

  , comCitrixXenclientXenmgrVmGetDomstoreReadAccess = getDomstoreReadAccess uuid
  , comCitrixXenclientXenmgrVmSetDomstoreReadAccess = \v -> restrict >> setDomstoreReadAccess uuid v
  , comCitrixXenclientXenmgrVmGetDomstoreWriteAccess = getDomstoreWriteAccess uuid
  , comCitrixXenclientXenmgrVmSetDomstoreWriteAccess = \v -> restrict >> setDomstoreWriteAccess uuid v

  , comCitrixXenclientXenmgrVmGetNativeExperience = getVmNativeExperience uuid
  , comCitrixXenclientXenmgrVmGetShowSwitcher = getVmShowSwitcher uuid
  , comCitrixXenclientXenmgrVmGetWirelessControl = getVmWirelessControl uuid
  , comCitrixXenclientXenmgrVmSetNativeExperience = restrict' $ setVmNativeExperience uuid
  , comCitrixXenclientXenmgrVmSetShowSwitcher = restrict' $ setVmShowSwitcher uuid
  , comCitrixXenclientXenmgrVmSetWirelessControl = restrict' $ setVmWirelessControl uuid

  , comCitrixXenclientXenmgrVmUnrestrictedGetNativeExperience = getVmNativeExperience uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetShowSwitcher = getVmShowSwitcher uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetWirelessControl = getVmWirelessControl uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetNativeExperience = setVmNativeExperience uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetShowSwitcher = setVmShowSwitcher uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetWirelessControl = setVmWirelessControl uuid

  , comCitrixXenclientXenmgrVmGetXciCpuidSignature = getVmXciCpuidSignature uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetXciCpuidSignature = getVmXciCpuidSignature uuid
  , comCitrixXenclientXenmgrVmSetXciCpuidSignature = restrict' $ setVmXciCpuidSignature uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetXciCpuidSignature = setVmXciCpuidSignature uuid

  , comCitrixXenclientXenmgrVmGetS3Mode = enumMarshall <$> getVmS3Mode uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetS3Mode = enumMarshall <$> getVmS3Mode uuid
  , comCitrixXenclientXenmgrVmSetS3Mode = restrict' $ setVmS3Mode uuid . enumMarshallReverse_
  , comCitrixXenclientXenmgrVmUnrestrictedSetS3Mode = setVmS3Mode uuid . enumMarshallReverse_

  , comCitrixXenclientXenmgrVmGetS4Mode = enumMarshall <$> getVmS4Mode uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetS4Mode = enumMarshall <$> getVmS4Mode uuid
  , comCitrixXenclientXenmgrVmSetS4Mode = restrict' $ setVmS4Mode uuid . enumMarshallReverse_
  , comCitrixXenclientXenmgrVmUnrestrictedSetS4Mode = setVmS4Mode uuid . enumMarshallReverse_

  , comCitrixXenclientXenmgrVmGetVsnd = getVmVsnd uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetVsnd = getVmVsnd uuid
  , comCitrixXenclientXenmgrVmSetVsnd = restrict' $ setVmVsnd uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetVsnd = setVmVsnd uuid

  , comCitrixXenclientXenmgrVmGetPrivateSpace = fromIntegral <$> getVmPrivateSpaceUsedMiB uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPrivateSpace = fromIntegral <$> getVmPrivateSpaceUsedMiB uuid

  , comCitrixXenclientXenmgrVmProductGetOvfEnvXml = liftIO (getOvfEnvXml uuid)
  , comCitrixXenclientXenmgrVmProductListProductProperties = map productPropertyKV <$> vmPPList uuid 
  , comCitrixXenclientXenmgrVmProductGetProductProperty = \p -> vmPPGetValue uuid (PPUniqueID p)
  , comCitrixXenclientXenmgrVmProductSetProductProperty = \p v -> vmPPSetValue uuid (PPUniqueID p) v

  , comCitrixXenclientXenmgrVmGetRealm = getVmRealm uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetRealm = getVmRealm uuid
  , comCitrixXenclientXenmgrVmSetRealm = restrict' $ setVmRealm uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetRealm = setVmRealm uuid
  , comCitrixXenclientXenmgrVmGetSyncUuid = getVmSyncUuid uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetSyncUuid = getVmSyncUuid uuid
  , comCitrixXenclientXenmgrVmSetSyncUuid = restrict' $ setVmSyncUuid uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetSyncUuid = setVmSyncUuid uuid

  , comCitrixXenclientXenmgrVmGetIcbinnPath = getVmIcbinnPath uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetIcbinnPath = getVmIcbinnPath uuid
  , comCitrixXenclientXenmgrVmSetIcbinnPath = restrict' $ setVmIcbinnPath uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetIcbinnPath = setVmIcbinnPath uuid

  , comCitrixXenclientXenmgrVmGetOvfTransportIso = getVmOvfTransportIso uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetOvfTransportIso = getVmOvfTransportIso uuid
  , comCitrixXenclientXenmgrVmSetOvfTransportIso = restrict' $ setVmOvfTransportIso uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetOvfTransportIso = setVmOvfTransportIso uuid

  , comCitrixXenclientXenmgrVmGetReady = getVmReady uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetReady = getVmReady uuid
  , comCitrixXenclientXenmgrVmSetReady = restrict' $ setVmReady uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetReady = setVmReady uuid

  , comCitrixXenclientXenmgrVmGetDownloadProgress = fromIntegral <$> getVmDownloadProgress uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetDownloadProgress = fromIntegral <$> getVmDownloadProgress uuid
  , comCitrixXenclientXenmgrVmSetDownloadProgress = restrict' $ (setVmDownloadProgress uuid . fromIntegral)
  , comCitrixXenclientXenmgrVmUnrestrictedSetDownloadProgress = setVmDownloadProgress uuid . fromIntegral

  , comCitrixXenclientXenmgrVmGetProvidesDefaultNetworkBackend = getVmProvidesDefaultNetworkBackend uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetProvidesDefaultNetworkBackend = getVmProvidesDefaultNetworkBackend uuid
  , comCitrixXenclientXenmgrVmSetProvidesDefaultNetworkBackend = restrict' $ setVmProvidesDefaultNetworkBackend uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetProvidesDefaultNetworkBackend = setVmProvidesDefaultNetworkBackend uuid

  , comCitrixXenclientXenmgrVmGetVkbd = getVmVkbd uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetVkbd = getVmVkbd uuid
  , comCitrixXenclientXenmgrVmSetVkbd = restrict' $ setVmVkbd uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetVkbd = setVmVkbd uuid

  , comCitrixXenclientXenmgrVmGetVfb = getVmVfb uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetVfb = getVmVfb uuid
  , comCitrixXenclientXenmgrVmSetVfb = restrict' $ setVmVfb uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetVfb = setVmVfb uuid

  , comCitrixXenclientXenmgrVmGetV4v = getVmV4V uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetV4v = getVmV4V uuid
  , comCitrixXenclientXenmgrVmSetV4v = restrict' $ setVmV4V uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetV4v = setVmV4V uuid

  , comCitrixXenclientXenmgrVmGetRestrictDisplayDepth = getVmRestrictDisplayDepth uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetRestrictDisplayDepth = getVmRestrictDisplayDepth uuid
  , comCitrixXenclientXenmgrVmSetRestrictDisplayDepth = restrict' $ setVmRestrictDisplayDepth uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetRestrictDisplayDepth = setVmRestrictDisplayDepth uuid

  , comCitrixXenclientXenmgrVmGetRestrictDisplayRes = getVmRestrictDisplayRes uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetRestrictDisplayRes = getVmRestrictDisplayRes uuid
  , comCitrixXenclientXenmgrVmSetRestrictDisplayRes = restrict' $ setVmRestrictDisplayRes uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetRestrictDisplayRes = setVmRestrictDisplayRes uuid

  , comCitrixXenclientXenmgrVmGetInitrdExtract = getVmInitrdExtract uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetInitrdExtract = getVmInitrdExtract uuid
  , comCitrixXenclientXenmgrVmSetInitrdExtract = restrict' $ setVmInitrdExtract uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetInitrdExtract = setVmInitrdExtract uuid

  , comCitrixXenclientXenmgrVmGetPreserveOnReboot = getVmPreserveOnReboot uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetPreserveOnReboot = getVmPreserveOnReboot uuid
  , comCitrixXenclientXenmgrVmSetPreserveOnReboot = restrict' $ setVmPreserveOnReboot uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetPreserveOnReboot = setVmPreserveOnReboot uuid

  , comCitrixXenclientXenmgrVmGetBootSentinel = fromMaybe "" <$> getVmBootSentinel uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetBootSentinel = fromMaybe "" <$> getVmBootSentinel uuid
  , comCitrixXenclientXenmgrVmSetBootSentinel = (restrict' $ setVmBootSentinel uuid) . stom
  , comCitrixXenclientXenmgrVmUnrestrictedSetBootSentinel = (setVmBootSentinel uuid) . stom

  , comCitrixXenclientXenmgrVmGetHpet = getVmHpet uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetHpet = getVmHpet uuid
  , comCitrixXenclientXenmgrVmSetHpet = restrict' $ setVmHpet uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetHpet = setVmHpet uuid

  , comCitrixXenclientXenmgrVmGetTimerMode = getVmTimerMode uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetTimerMode = getVmTimerMode uuid
  , comCitrixXenclientXenmgrVmSetTimerMode = (restrict' $ setVmTimerMode uuid)
  , comCitrixXenclientXenmgrVmUnrestrictedSetTimerMode = (setVmTimerMode uuid)

  , comCitrixXenclientXenmgrVmGetNestedhvm = getVmNestedHvm uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetNestedhvm = getVmNestedHvm uuid
  , comCitrixXenclientXenmgrVmSetNestedhvm = restrict' $ setVmNestedHvm uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetNestedhvm = setVmNestedHvm uuid

  , comCitrixXenclientXenmgrVmGetSerial = getVmSerial uuid
  , comCitrixXenclientXenmgrVmUnrestrictedGetSerial = getVmSerial uuid
  , comCitrixXenclientXenmgrVmSetSerial = restrict' $ setVmSerial uuid
  , comCitrixXenclientXenmgrVmUnrestrictedSetSerial = setVmSerial uuid

  } where
    stom "" = Nothing
    stom x  = Just x
    restrict :: Rpc ()
    restrict =
        do can <- policyQueryModifyVmSettings uuid
           if can
              then return ()
              else failCannotEditPropertiesDueToPolicy

    restrict' :: (a -> Rpc ()) -> (a -> Rpc ())
    restrict' f = \v -> restrict >> f v

    getruncmd getter = fromMaybe "" <$> getter uuid
    setruncmd setter "" = setter uuid Nothing
    setruncmd setter v  = setter uuid (Just v)

productPropertyKV :: ProductProperty -> M.Map String String
productPropertyKV pp = M.fromList [
    ("id", strPPUniqueID (ppUniqueID pp))
  , ("type", strPPT (ppType pp))  
  , ("key", ppKey pp)
  , ("class", ppClass pp)
  , ("instance", ppInstance pp)
  , ("label", "") -- FIXME
  , ("description", ppDescription pp)
  , ("user-configurable", of_bool (ppUserConfigurable pp))
  , ("password", of_bool (ppPassword pp))
  ]
  where
    of_bool True = "true"
    of_bool _ = "false"

_auth_required uuid = isLoginRequired uuid
_auth          uuid = loginToVm uuid

_list_pt_rules uuid =
    do pcis <- M.toList <$> getPciPtRules uuid
       let pcis'   = sortBy compIDS pcis
           entries = map dump pcis'
       return $ entries
    where
      compIDS a b    = compare (fst a) (fst b)
      dump (id,rule) = M.fromList [ ("id", show id)
                                  , ("rule", show rule) ]

_list_pt_pci_devices :: Uuid -> Rpc [M.Map String String]
_list_pt_pci_devices uuid =
    mapM device_info =<< getPciPtDevices uuid
    where
      device_info (PciPtDev { pciPtDevice=d }) = do
        inf <- liftIO $ pciGetInfo d
        let cls = fromMaybe "" ((printf "0x%X" . pciinfoClass) `fmap` inf)
            ven = fromMaybe "" ((printf "0x%X" . pciinfoVendor) `fmap` inf)
            did = fromMaybe "" ((printf "0x%X" . pciinfoDevice) `fmap` inf)
        return $ M.fromList [ ("addr", show (devAddr d))
                            , ("name", devName d)
                            , ("class", cls)
                            , ("vendor-id", ven)
                            , ("device-id", did)
                            ]

_add_pt_rule     uuid cls vendor_id device_id = modifyVmPciPtRules uuid $ pciAddRule (form_rule (cls,device_id,vendor_id))
_delete_pt_rule  uuid cls vendor_id device_id = modifyVmPciPtRules uuid $ pciDelRule (form_rule (cls,device_id,vendor_id))
_add_pt_rule_bdf     uuid bdf                 = modifyVmPciPtRules uuid $ pciAddRule (form_rule_bdf bdf)
_delete_pt_rule_bdf  uuid bdf                 = modifyVmPciPtRules uuid $ pciDelRule (form_rule_bdf bdf)

form_rule (cls,device_id,vendor_id) =
    PciPtRule { ruleClass = perhaps cls read
              , ruleDevice = perhaps device_id read
              , ruleVendor = perhaps vendor_id read
              , ruleForceSlot = False }
    where perhaps "" _  = Nothing
          perhaps "any" _ = Nothing
          perhaps str f = Just (f str)

form_rule_bdf = rule . fromMaybe (error "error parsing rule") . pciAndSlotFromStr where
  rule (addr,sl) = PciPtRuleBDF addr sl

_add_disk uuid =
    do id <- xmRunVm uuid $ addDefaultDiskToVm uuid
       VmDiskObj.expose uuid id
       return $ diskObjectPath uuid id

_list_disks uuid =
    do disk_map <- getDisks uuid
       let disk_ids = sort $ M.keys disk_map
           paths    = map form_path disk_ids
       return paths
    where
      form_path disk_id = diskObjectPath uuid disk_id

_add_nic uuid =
    do id <- xmRunVm uuid $ addDefaultNicToVm uuid
       VmNicObj.expose uuid id
       return $ nicObjectPath uuid id

_list_nics uuid =
    do nics <- getVmNicDefs' uuid
       let nic_ids = sort $ map nicdefId nics
       return $ map form_path nic_ids
    where
      form_path nic_id = nicObjectPath uuid nic_id

dbpath uuid = "/vm/" ++ show uuid
domstorepath uuid = "/dom-store/" ++ show uuid
slashdot = replace "." "/"

_get_db_key uuid key = fromMaybe "" <$> dbMaybeRead (dbpath uuid ++ "/" ++ slashdot key)
_set_db_key uuid key value = dbWrite (dbpath uuid ++ "/" ++ replace "." "/" key) value
_get_domstore_key uuid key = fromMaybe "" <$> dbMaybeRead (domstorepath uuid ++ "/" ++ slashdot key)
_set_domstore_key uuid key value = dbWrite (domstorepath uuid ++ "/" ++ replace "." "/" key) value

_type_of_str "pvm" = Svm
_type_of_str "svm" = Svm
_type_of_str tag   = ServiceVm tag

_type_to_str Svm = "svm"
_type_to_str (ServiceVm tag) = tag

--FIXME: make this less hackish
_state_str :: Vm String
_state_str = vmUuid >>= \uuid -> go uuid where
    go uuid = from =<< getVmState where
      from Shutdown = fromShutdown =<< liftRpc (isLoginRequired uuid)
      from s = return . stateToPublicStr $ s
      fromShutdown False = return . stateToPublicStr $ Shutdown
      fromShutdown True  = return eVM_STATE_LOCKED

_create_child_service_vm :: Uuid -> String -> XM ObjectPath
_create_child_service_vm parent_uuid template =
    liftIO (getChildServiceVmTemplate template) >>= f where
        f t = do
          service <- (createVm False) $ defaultCreateVmPms { cvmUuid = Nothing
                                                   , cvmTemplate = Just ("child-" ++ template)
                                                   , cvmAutoSlot = False }
          info $ "applying template overrides from " ++ template ++ " to vm " ++ show parent_uuid
          liftRpc $ applyChildTemplateToVm (t,service) parent_uuid
          expose service
          liftRpc $ notifyVmCreated service
          return $ vmObjPath service

_list_v4v_firewall_rules :: Uuid -> Rpc [String]
_list_v4v_firewall_rules uuid = map Firewall.ruleToString <$> getVmFirewallRules uuid

_add_v4v_firewall_rule :: Uuid -> String -> Rpc ()
_add_v4v_firewall_rule uuid rule_str =
    case Firewall.parseRule rule_str of
      Nothing -> failRuleParseError
      Just r  -> addVmFirewallRule uuid r

_delete_v4v_firewall_rule :: Uuid -> String -> Rpc ()
_delete_v4v_firewall_rule uuid rule_str =
    case Firewall.parseRule rule_str of
      Nothing -> failRuleParseError
      Just r  -> deleteVmFirewallRule uuid r

firerulePath :: Uuid -> Int32 -> String
firerulePath uuid ruleid = "/vm/" ++ show uuid ++ "/firewall-rule/" ++ show ruleid

_add_firewall_rule :: Uuid -> Int32 -> String -> String -> String -> Rpc ()
_add_firewall_rule uuid ruleid dir ip extra = do
  when (not $ dir `elem` ["in", "out"]) $ error "bad direction - needs to be 'in' or 'out'"
  unlessM (policyQueryModifyVmSettings uuid) failCannotEditPropertiesDueToPolicy
  w "direction" dir
  w "remote-ip" ip
  w "extra" extra
  where
    w p = dbWrite (firerulePath uuid ruleid ++ "/" ++ p)

_delete_firewall_rule :: Uuid -> Int32 -> Rpc ()
_delete_firewall_rule uuid ruleid = do
  unlessM (policyQueryModifyVmSettings uuid) failCannotEditPropertiesDueToPolicy
  dbRm (firerulePath uuid ruleid)

_list_firewall_rules :: Uuid -> Rpc [M.Map String String]
_list_firewall_rules uuid = mapM entry =<< dbList ("/vm/" ++ show uuid ++ "/firewall-rule")
  where
    entry :: String -> Rpc (M.Map String String)
    entry ruleid = do
      dir <- r "direction"
      ip  <- r "remote-ip"
      extra <- r "extra"
      return $ M.fromList $ [ ("id", ruleid)
                            , ("direction", dir)
                            , ("remote-ip", ip)
                            , ("extra", extra) ]
      where
        r :: String -> Rpc String
        r p = dbRead (firerulePath uuid (read ruleid) ++ "/" ++ p)
  
getDomstoreReadAccess uuid = readConfigPropertyDef uuid vmDomstoreReadAccess False
getDomstoreWriteAccess uuid = readConfigPropertyDef uuid vmDomstoreWriteAccess False

setDomstoreReadAccess uuid = saveConfigProperty uuid vmDomstoreReadAccess
setDomstoreWriteAccess uuid = saveConfigProperty uuid vmDomstoreWriteAccess
