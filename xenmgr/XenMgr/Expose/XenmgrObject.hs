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

module XenMgr.Expose.XenmgrObject (expose) where

import Control.Monad
import Control.Applicative
import Data.String
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)
import Data.Int
import System.FilePath.Posix

import Tools.File
import Tools.Misc
import Tools.Log

import XenMgr.Rpc
import XenMgr.Db
import Rpc.Autogen.XenmgrServer
import qualified XenMgr.Expose.VmObject
import qualified Rpc.Autogen.XenmgrServer as S
import Vm.Types
import Vm.Queries
import Vm.Actions
import Vm.Policies
import Vm.Pci
import Vm.Monad
import XenMgr.PowerManagement
import XenMgr.Config
import XenMgr.Diagnostics
import XenMgr.Testing
import XenMgr.Notify
import XenMgr.Expose.ObjectPaths
import Rpc.Autogen.XenmgrNotify
import Vm.Templates
import XenMgr.XM
import XenMgr.Host
import XenMgr.HostOps
import XenMgr.GuestReq

expose :: TestingContext -> XM ()
expose testingCtx = do
    info "exposing xenmgr object"
    xm <- xmContext
    liftRpc $ rpcExpose xenmgrObjectPath (S.interfaces $ implementation xm testingCtx)

-- just some utility used below to convert function working on Uuids into function working on string
-- (as uuids get passed as strings through rpc calls)

implementation xm testingCtx =
  let cc = notifyComCitrixXenclientXenmgrConfigChanged xenmgrObjectPath in
  XenmgrServer {
    comCitrixXenclientXenmgrListVms = _ListVms
  , comCitrixXenclientXenmgrListDomids = _ListDomids
  , comCitrixXenclientXenmgrListTemplates = liftIO enumTemplateTags
  , comCitrixXenclientXenmgrListUiTemplates = _ListUiTemplates                                            
  , comCitrixXenclientXenmgrListChildServiceVmTemplates = _ListChildServiceVmTemplates
  , comCitrixXenclientXenmgrListExtensionPacks = _ListExtensionPacks
  , comCitrixXenclientXenmgrFindVmByUuid = _FindVmByUuid
  , comCitrixXenclientXenmgrFindVmByDomid = _FindVmByDomid
  , comCitrixXenclientXenmgrCreateVm = _CreateVm False xm defaultCreateVmPms
  , comCitrixXenclientXenmgrCreateVmWithTemplate = \template -> _CreateVm False xm (defaultCreateVmPms { cvmTemplate = Just template })
  , comCitrixXenclientXenmgrCreateVmWithTemplateAndUuid = \template uuid -> _CreateVm False xm (defaultCreateVmPms { cvmTemplate = Just template, cvmUuid = Just (fromString uuid) })
  , comCitrixXenclientXenmgrCreateVmWithTemplateAndJson = \template json -> _CreateVm False xm (defaultCreateVmPms { cvmTemplate = Just template, cvmExtraJson = json })
  , comCitrixXenclientXenmgrCreateVmWithUi = \template name description imagepath ->
                                               _CreateVm False xm
                                                 (defaultCreateVmPms { cvmTemplate = Just template
                                                                     , cvmName = Just name
                                                                     , cvmDescription = Just description
                                                                     , cvmImagePath = Just imagepath } )
  , comCitrixXenclientXenmgrCreateVhd = _CreateVhd
  , comCitrixXenclientXenmgrConfigGetIsoPath = appIsoPath
  , comCitrixXenclientXenmgrConfigSetIsoPath = \v -> appSetIsoPath v >> cc
  , comCitrixXenclientXenmgrConfigGetAutostart = appAutoStart
  , comCitrixXenclientXenmgrConfigSetAutostart = \v -> appSetAutoStart v >> cc
  , comCitrixXenclientXenmgrConfigGetPvmAutostartDelay = fromIntegral <$> appPvmAutoStartDelay
  , comCitrixXenclientXenmgrConfigSetPvmAutostartDelay = \d -> appSetPvmAutoStartDelay (fromIntegral d) >> cc
  , comCitrixXenclientXenmgrConfigGetSvmAutostartDelay = fromIntegral <$> appSvmAutoStartDelay
  , comCitrixXenclientXenmgrConfigSetSvmAutostartDelay = \d -> appSetSvmAutoStartDelay (fromIntegral d) >> cc
  , comCitrixXenclientXenmgrConfigGetArgoHostsFile = appArgoHostsFile
  , comCitrixXenclientXenmgrConfigSetArgoHostsFile = \v -> appSetArgoHostsFile v >> cc
  , comCitrixXenclientXenmgrConfigGetUseNetworkingDomain = return True
  , comCitrixXenclientXenmgrConfigGetBypassSha1sumChecks = appBypassSha1SumChecks
  , comCitrixXenclientXenmgrConfigGetXcDiagTimeout = fromIntegral <$> appXcDiagTimeout
  , comCitrixXenclientXenmgrConfigSetXcDiagTimeout = \t -> appSetXcDiagTimeout (fromIntegral t) >> cc
  , comCitrixXenclientXenmgrConfigUiGetIdleTimeThreshold = fromIntegral <$> appIdleTimeThreshold
  , comCitrixXenclientXenmgrConfigUiSetIdleTimeThreshold = \t -> appSetIdleTimeThreshold (fromIntegral t) >> cc
  , comCitrixXenclientXenmgrConfigGetPlatformCryptoKeyDirs = appGetPlatformCryptoKeyDirs
  , comCitrixXenclientXenmgrConfigSetPlatformCryptoKeyDirs = \v -> appSetPlatformCryptoKeyDirs v >> cc
  , comCitrixXenclientXenmgrConfigGetGuestOnlyNetworking = appGetGuestOnlyNetworking
  , comCitrixXenclientXenmgrConfigGetVmCreationAllowed = policyQueryVmCreation
  , comCitrixXenclientXenmgrConfigSetVmCreationAllowed = \v -> policySetVmCreation v >> cc
  , comCitrixXenclientXenmgrConfigGetVmDeletionAllowed = policyQueryVmDeletion
  , comCitrixXenclientXenmgrConfigSetVmDeletionAllowed = \v -> policySetVmDeletion v >> cc
  , comCitrixXenclientXenmgrConfigGetOtaUpgradesAllowed = policyQueryOtaUpgrades
  , comCitrixXenclientXenmgrConfigSetOtaUpgradesAllowed = \v -> policySetOtaUpgrades v >> cc
  , comCitrixXenclientXenmgrConfigGetConnectRemoteDesktopAllowed = fromMaybe True <$> dbMaybeRead "/xenmgr/uiConnectRemoteDesktopAllowed"
  , comCitrixXenclientXenmgrConfigSetConnectRemoteDesktopAllowed = \v -> dbWrite "/xenmgr/uiConnectRemoteDesktopAllowed" v >> cc
  , comCitrixXenclientXenmgrConfigGetMeasureFailAction = pmActionToStr <$> getMeasureFailAction
  , comCitrixXenclientXenmgrConfigSetMeasureFailAction = \v -> setMeasureFailAction (pmActionOfStr v) >> cc

  , comCitrixXenclientXenmgrConfigGetArgoFirewall = appGetArgoFirewall
  , comCitrixXenclientXenmgrConfigSetArgoFirewall = appSetArgoFirewall
  , comCitrixXenclientXenmgrConfigGetSecondaryGpuPt = appMultiGpuPt
  , comCitrixXenclientXenmgrConfigGetConfigurableSaveChangesAcrossReboots = appConfigurableSaveChangesAcrossReboots

  , comCitrixXenclientXenmgrConfigGetEnableSsh = appGetEnableSsh
  , comCitrixXenclientXenmgrConfigSetEnableSsh = appSetEnableSsh
  , comCitrixXenclientXenmgrConfigGetEnableArgoSsh = appGetEnableArgoSsh
  , comCitrixXenclientXenmgrConfigSetEnableArgoSsh = appSetEnableArgoSsh
  , comCitrixXenclientXenmgrConfigGetEnableDom0Networking = appGetEnableDom0Networking
  , comCitrixXenclientXenmgrConfigSetEnableDom0Networking = appSetEnableDom0Networking

  , comCitrixXenclientXenmgrConfigGetDom0MemTargetMib = fromIntegral <$> appGetDom0MemTargetMIB
  , comCitrixXenclientXenmgrConfigSetDom0MemTargetMib = \v -> appSetDom0MemTargetMIB (fromIntegral v)

  , comCitrixXenclientXenmgrConfigGetAutolockCdDrives = appGetAutolockCdDrives
  , comCitrixXenclientXenmgrConfigSetAutolockCdDrives = hostChangeAutolockCdDrives

  , comCitrixXenclientXenmgrConfigUiGetSwitcherEnabled = appGetSwitcherEnabled
  , comCitrixXenclientXenmgrConfigUiSetSwitcherEnabled = appSetSwitcherEnabled
  , comCitrixXenclientXenmgrConfigUiGetSwitcherSelfSwitchEnabled = appGetSwitcherSelfSwitchEnabled
  , comCitrixXenclientXenmgrConfigUiSetSwitcherSelfSwitchEnabled = appSetSwitcherSelfSwitchEnabled
  , comCitrixXenclientXenmgrConfigUiGetSwitcherKeyboardFollowsMouse = appGetSwitcherKeyboardFollowsMouse
  , comCitrixXenclientXenmgrConfigUiSetSwitcherKeyboardFollowsMouse = appSetSwitcherKeyboardFollowsMouse
  , comCitrixXenclientXenmgrConfigUiGetSwitcherResistance = fromIntegral <$> appGetSwitcherResistance
  , comCitrixXenclientXenmgrConfigUiSetSwitcherResistance = appSetSwitcherResistance . fromIntegral
  , comCitrixXenclientXenmgrConfigUiGetSwitcherStatusReportEnabled = appGetSwitcherStatusReportEnabled
  , comCitrixXenclientXenmgrConfigUiSetSwitcherStatusReportEnabled = appSetSwitcherStatusReportEnabled

  , comCitrixXenclientXenmgrConfigUiGetSupportedLanguages = appGetSupportedLanguages
  , comCitrixXenclientXenmgrConfigUiGetLanguage = appGetLanguage
  , comCitrixXenclientXenmgrConfigUiSetLanguage = \v -> appSetLanguage v >> cc >> notifyComCitrixXenclientXenmgrLanguageChanged xenmgrObjectPath

  , comCitrixXenclientXenmgrConfigUiGetShowMsgOnVmStart = fromMaybe True <$> dbMaybeRead "/xenmgr/showMsgOnVmStart"
  , comCitrixXenclientXenmgrConfigUiSetShowMsgOnVmStart = \v -> dbWrite "/xenmgr/showMsgOnVmStart" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetShowMsgOnVmStartToolsWarning = fromMaybe True <$> dbMaybeRead "/xenmgr/showMsgOnVmStartToolsWarning"
  , comCitrixXenclientXenmgrConfigUiSetShowMsgOnVmStartToolsWarning = \v -> dbWrite "/xenmgr/showMsgOnVmStartToolsWarning" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetShowMsgOnNoDisk = fromMaybe True <$> dbMaybeRead "/xenmgr/showMsgOnNoDisk"
  , comCitrixXenclientXenmgrConfigUiSetShowMsgOnNoDisk = \v -> dbWrite "/xenmgr/showMsgOnNoDisk" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetShowToolsWarning = fromMaybe True <$> dbMaybeRead "/xenmgr/showToolsWarning"
  , comCitrixXenclientXenmgrConfigUiSetShowToolsWarning = \v -> dbWrite "/xenmgr/showToolsWarning" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetShowMbootWarning = fromMaybe True <$> dbMaybeRead "/xenmgr/showMbootWarning"
  , comCitrixXenclientXenmgrConfigUiSetShowMbootWarning = \v -> dbWrite "/xenmgr/showMbootWarning" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetPointerTrailTimeout = round . (*1000) . fromMaybe (0::Double) <$> dbMaybeRead "/xenmgr/pointerTrailTimeout"
  , comCitrixXenclientXenmgrConfigUiSetPointerTrailTimeout = \v -> dbWrite "/xenmgr/pointerTrailTimeout" ((fromIntegral v :: Double) / 1000) >> cc
  , comCitrixXenclientXenmgrConfigUiGetWallpaper = fromMaybe "images/wallpapers/s9.png" <$> dbMaybeRead "/xenmgr/wallpaper"
  , comCitrixXenclientXenmgrConfigUiSetWallpaper = \v -> dbWrite "/xenmgr/wallpaper" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetViewType = fromMaybe "" <$> dbMaybeRead "/xenmgr/viewType"
  , comCitrixXenclientXenmgrConfigUiSetViewType = \v -> dbWrite "/xenmgr/viewType" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetModifySettings = fromMaybe True <$> dbMaybeRead "/xenmgr/uiModifySettings"
  , comCitrixXenclientXenmgrConfigUiSetModifySettings = \v -> dbWrite "/xenmgr/uiModifySettings" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetModifyAdvancedVmSettings = fromMaybe True <$> dbMaybeRead "/xenmgr/uiModifyAdvancedVmSettings"
  , comCitrixXenclientXenmgrConfigUiSetModifyAdvancedVmSettings = \v -> dbWrite "/xenmgr/uiModifyAdvancedVmSettings" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetModifyServices = fromMaybe True <$> dbMaybeRead "/xenmgr/uiModifyServices"
  , comCitrixXenclientXenmgrConfigUiSetModifyServices = \v -> dbWrite "/xenmgr/uiModifyServices" v >> cc
  , comCitrixXenclientXenmgrConfigUiGetModifyUsbSettings = fromMaybe True <$> dbMaybeRead "/xenmgr/uiModifyUsbSettings"
  , comCitrixXenclientXenmgrConfigUiSetModifyUsbSettings = \v -> dbWrite "/xenmgr/uiModifyUsbSettings" v >> cc
    
  , comCitrixXenclientPolicyEnforce    = \uuid -> policiesEnforce (fromString uuid)
  , comCitrixXenclientPolicyRetrieve   = \uuid -> policiesRetrieve (fromString uuid)
  , comCitrixXenclientXenmgrDiagSave   = \mode -> diagSave mode
  , comCitrixXenclientXenmgrDiagGather = diagGather
  , comCitrixXenclientXenmgrDiagCreateStatusReport = \a b c d e f -> liftIO (diagStatusReport a b c d e f)
  , comCitrixXenclientXenmgrDiagStatusReportScreen = diagStatusReportScreen
  , comCitrixXenclientXenmgrDiagTaasAuthenticateCredentials = \a b -> (diagTaasAuthenticateCredentials a b )
  , comCitrixXenclientXenmgrDiagTaasUpload = \a b c d -> (diagTaasUpload a b c d)
  , comCitrixXenclientXenmgrDiagTaasAgreeTerms = \a b c  -> diagTaasAgreeTerms a b c
  , comCitrixXenclientXenmgrTestingScriptQueue   = \s -> liftIO $ testingQueueScript testingCtx s
  , comCitrixXenclientXenmgrTestingScriptDequeue = liftIO $ testingDequeueScript testingCtx
  , comCitrixXenclientXenmgrUnrestrictedUnrestrictedCreateVm = _CreateVm True xm defaultCreateVmPms
  , comCitrixXenclientXenmgrUnrestrictedUnrestrictedCreateVmWithTemplateAndJson = \template json -> _CreateVm True xm (defaultCreateVmPms { cvmTemplate = Just template, cvmExtraJson = json })
  , comCitrixXenclientXenmgrUnrestrictedUnrestrictedDeleteVm = \uuid -> removeVm (fromString uuid)
                                                                        
  , comCitrixXenclientXenmgrGuestreqRequestAttention = guestRequestAttention
  }

_ListVms :: Rpc [ObjectPath]
_ListVms = getVms >>= return . map vmObjPath

_ListDomids :: Rpc [Int32]
_ListDomids = getVms >>= mapM getDomainID >>= return . catMaybes >>= return . map fromIntegral

_ListExtensionPacks :: Rpc [Map String String]
_ListExtensionPacks = map entry <$> getExtPacks where
  entry e
    = M.fromList [ ("id", extpackId e)
                 , ("vendor", extpackVendor e)
                 , ("name", extpackName e)
                 , ("description", extpackDescr e)
                 , ("version", extpackVersion e)
                 , ("image", extpackImage e)
                 ]

_FindVmByUuid :: String -> Rpc ObjectPath
_FindVmByUuid uuid_str =
    do let uuid = fromString uuid_str
       exists <- dbExists $ "/vm/" ++ uuid_str
       case exists of
         True  -> return $ vmObjPath uuid
         False -> error  $ "no vm with uuid " ++ uuid_str

_FindVmByDomid :: Int32 -> Rpc ObjectPath
_FindVmByDomid domid =
    do maybe_uuid <- getVmByDomid (fromIntegral domid)
       case maybe_uuid of
         Just uuid -> return $ vmObjPath uuid
         Nothing   -> error $ "no vm with domid " ++ show domid

_CreateVm :: Bool -> XmContext -> CreateVmPms -> Rpc ObjectPath
_CreateVm unrestricted xm pms =
    do uuid <- runXM xm $ createVm unrestricted pms
       -- after successful creation, put it on DBUS
       runXM xm $ XenMgr.Expose.VmObject.expose uuid
       runEventScript HardFail uuid getVmRunPostCreate [uuidStr uuid]
       notifyVmCreated uuid
       -- and return the path to the newly created object
       return $ vmObjPath uuid

_CreateVhd :: Int32 -> Rpc String
_CreateVhd sizeMB = liftIO $ createVhd (fromIntegral sizeMB)

_ListChildServiceVmTemplates :: Rpc [String]
_ListChildServiceVmTemplates = liftIO enumChildServiceVmTags

_ListUiTemplates :: Rpc [Map String String]
_ListUiTemplates = map mk `fmap` liftIO enumUiTemplates where
  mk (tag, description) = M.fromList [("template", tag), ("description", description)]
