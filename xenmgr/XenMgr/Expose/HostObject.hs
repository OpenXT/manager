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

{-# LANGUAGE PatternGuards #-}
module XenMgr.Expose.HostObject (expose) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Applicative
import Data.Maybe
import Data.Char
import Data.Int
import Data.List (isPrefixOf)
import Data.String
import Data.Bits
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Text.Printf
import Text.Regex.Posix

import Tools.Log
import Tools.Misc
import Tools.XenStore
import Tools.File
import Tools.Process
import Tools.Text
import System.FilePath
import System.Posix.Time
import Locale
import Data.Time
import Directory
import System.Directory (canonicalizePath)

import XenMgr.XM
import Vm.Types
import Vm.Queries
import Vm.Actions
import Vm.Pci
import Vm.Policies

import XenMgr.Db
import XenMgr.Config
import XenMgr.Host
import XenMgr.HostOps
import XenMgr.PowerManagement
import XenMgr.FetchLog
import XenMgr.CdLock
import Rpc.Autogen.XenmgrHostServer
import XenMgr.Expose.ObjectPaths
import XenMgr.Connect.InputDaemon
import Vm.Balloon

import Rpc.Autogen.XenmgrNotify

getUsableMemMib :: Rpc Int
getUsableMemMib = (+) <$> liftIO getFreeMem <*> overused where
  overused = fromIntegral . kibToMib <$> getTotalOveruseKib

expose :: CachedHostInfo -> XM ()
expose host_info = do
    info "exposing host object"
    xm  <- xmContext
    imp <- implementation xm host_info
    liftRpc $ rpcExpose hostObjectPath (interfaces imp)

-- wrapped into IO monad since some state is shared between methods (fetch log ones)

implementation xm host_info_cache = do
  fetchState <- liftIO $ fetchLogInitState
  return XenmgrHostServer {
    comCitrixXenclientXenmgrPowersettingsGetAcLidCloseAction = pmGetSettings >>= return . pmActionToStr . pmLidCloseACAction
  , comCitrixXenclientXenmgrPowersettingsGetBatteryLidCloseAction = pmGetSettings >>= return . pmActionToStr . pmLidCloseBatteryAction
  , comCitrixXenclientXenmgrPowersettingsSetAcLidCloseAction = \act -> pmSaveAcLidCloseAction (pmActionOfStr act)
  , comCitrixXenclientXenmgrPowersettingsSetBatteryLidCloseAction = \act -> pmSaveBatteryLidCloseAction (pmActionOfStr act)

  , comCitrixXenclientXenmgrHostListIsos = _ListIso
  , comCitrixXenclientXenmgrHostListPciDevices = _ListPciDevices
  , comCitrixXenclientXenmgrHostListGpuDevices = _ListGpuDevices
  , comCitrixXenclientXenmgrHostListDiskDevices = _ListDiskDevices
  , comCitrixXenclientXenmgrHostListUiPlugins = _ListUiPlugins
  , comCitrixXenclientXenmgrHostListCdDevices = _ListCdDevices
  , comCitrixXenclientXenmgrHostGetCdDeviceAssignment = _GetCdDeviceAssignment
  , comCitrixXenclientXenmgrHostAssignCdDevice = _AssignCdDevice
  , comCitrixXenclientXenmgrHostEjectCdDevice = _EjectCdDevice
  , comCitrixXenclientXenmgrHostIsServiceRunning = isServiceRunning

  , comCitrixXenclientXenmgrHostShutdown  = runXM xm hostShutdown
  , comCitrixXenclientXenmgrHostReboot    = runXM xm hostReboot
  , comCitrixXenclientXenmgrHostSleep     = runXM xm hostSleep
  , comCitrixXenclientXenmgrHostHibernate = runXM xm hostHibernate

  , comCitrixXenclientXenmgrHostGetState = hostStatusForUI <$> getHostState

  , comCitrixXenclientXenmgrHostGetTotalMem     = fromIntegral <$> liftIO getTotalMem
  , comCitrixXenclientXenmgrHostGetFreeMem      = fromIntegral <$> liftIO getFreeMem
  , comCitrixXenclientXenmgrHostGetAvailMem     = fromIntegral <$> getUsableMemMib
  , comCitrixXenclientXenmgrHostGetFreeStorage  = fromIntegral <$> liftIO getFreeStorage
  , comCitrixXenclientXenmgrHostGetTotalStorage = fromIntegral <$> liftIO getTotalStorage
  , comCitrixXenclientXenmgrHostGetCpuCount     = fromIntegral <$> liftIO getCpuCount
  , comCitrixXenclientXenmgrHostGetMeasuredBootEnabled = liftIO $ doesFileExist "/boot/system/tpm/enabled"
  , comCitrixXenclientXenmgrHostGetMeasuredBootSuccessful = liftIO $ doesFileExist "/config/tpm/measured-boot"
  , comCitrixXenclientXenmgrHostGetSystemAmtPt = haveSystemAmtPt
  , comCitrixXenclientXenmgrHostGetLaptop = _host_field hostIsLaptop
  , comCitrixXenclientXenmgrHostGetIsLicensed = liftIO hostIsLicensed
  
  , comCitrixXenclientXenmgrHostGetAmtCapable = _host_field hostIsAmtCapable
  , comCitrixXenclientXenmgrHostGetModel = _host_field hostModel
  , comCitrixXenclientXenmgrHostGetVendor = _host_field hostVendor
  , comCitrixXenclientXenmgrHostGetSerial = _host_field hostSerial
  , comCitrixXenclientXenmgrHostGetBiosRevision = _host_field hostBiosRev
  , comCitrixXenclientXenmgrHostGetEth0Mac = fromMaybe "" <$> liftIO eth0Mac
  , comCitrixXenclientXenmgrHostGetEth0Model = liftIO getEth0Model
  , comCitrixXenclientXenmgrHostGetWirelessMac = fromMaybe "" <$> liftIO wlan0Mac
  , comCitrixXenclientXenmgrHostGetWirelessModel = liftIO getWlan0Model
  , comCitrixXenclientXenmgrHostGetPhysicalCpuModel = _host_field hostPhysCpuModel
  , comCitrixXenclientXenmgrHostGetPhysicalGpuModel = _host_field hostPhysGpuModel
  , comCitrixXenclientXenmgrHostGetSafeGraphics = _GetSafeGraphics
  , comCitrixXenclientXenmgrHostGetBuildInfo = _GetBuildInfo
  , comCitrixXenclientXenmgrHostGetUiReady = _GetUiReady
  , comCitrixXenclientXenmgrHostSetUiReady = _SetUiReady
  , comCitrixXenclientXenmgrHostGetPlaybackPcm = pcmDeviceIdStr <$> getSelectedHostPlaybackDevice
  , comCitrixXenclientXenmgrHostGetCapturePcm = pcmDeviceIdStr <$> getSelectedHostCaptureDevice
  , comCitrixXenclientXenmgrHostSetPlaybackPcm = selectHostPlaybackDevice . PcmDeviceId
  , comCitrixXenclientXenmgrHostSetCapturePcm = selectHostCaptureDevice . PcmDeviceId

  , comCitrixXenclientXenmgrHostListPlaybackDevices = map pcmDeviceKV <$> liftIO getHostPlaybackDevices
  , comCitrixXenclientXenmgrHostListCaptureDevices = map pcmDeviceKV <$> liftIO getHostCaptureDevices

  , comCitrixXenclientXenmgrHostListSoundCards = listSoundCards
  , comCitrixXenclientXenmgrHostListSoundCardControls = listSoundCardControls
  , comCitrixXenclientXenmgrHostGetSoundCardControl = getSoundCardControl
  , comCitrixXenclientXenmgrHostSetSoundCardControl = setSoundCardControl
  
  , comCitrixXenclientXenmgrHostSetLicense = \date dev hash -> hostSetLicense date dev hash
  , comCitrixXenclientXenmgrHostGetGpuPlacement = _GetGpuPlacement
  , comCitrixXenclientXenmgrHostConfigureGpuPlacement = _ConfigureGpuPlacement
  , comCitrixXenclientXenmgrHostGetSecondsFromEpoch = _GetSecondsFromEpoch
  , comCitrixXenclientXenmgrInstallerGetEula              = getEULA
  , comCitrixXenclientXenmgrInstallerGetInstallstate      = _GetInstallstate
  , comCitrixXenclientXenmgrInstallerProgressInstallstate = _ProgressInstallstate
  , comCitrixXenclientXenmgrHostGetDisplayhandlerGpu = getDisplayHandlerGpu
  , comCitrixXenclientXenmgrHostSetDisplayhandlerGpu = \gpu -> setDisplayHandlerGpu gpu
  , comCitrixXenclientXenmgrHostListGpusForDisplayhandler = _ListGpusForDisplayhandler
  }

  where
    -- Helper to access host information field
    _host_field field =
        do hi <- liftIO $ getHostInfo host_info_cache
           return $ field hi

hostStatusForUI :: HostState -> String
hostStatusForUI HostIdle = "idle"
hostStatusForUI HostShuttingDown = "shutdowning"
hostStatusForUI HostRebooting = "rebooting"
hostStatusForUI HostGoingToSleep = "sleeping"
hostStatusForUI HostGoingToHibernate = "hibernating"

dateStr :: UTCTime -> String
dateStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
  
pcmDeviceKV :: PcmDevice -> Map String String
pcmDeviceKV d = M.fromList [
    ("id", pcmDeviceIdStr (pcmId d))
  , ("name", pcmName d)
  , ("card", fromMaybe "" (cardFromId $ pcmDeviceIdStr (pcmId d)))
  ]
  where
    cardFromId x
      | [a,b] <- split '-' x = show <$> (maybeRead a :: Maybe Int)
      | otherwise = Nothing

{-
memStr :: Int -> String
memStr mb = show mb ++ " MB"

storeStr :: Int -> String
storeStr mb = let gb = (fromIntegral mb) / 1024.0 :: Double in
              if gb >= 1.0
              then printf "%.02f GB" gb
              else show mb ++ " MB"
-}

_GetUiReady :: Rpc Bool
_GetUiReady = liftIO $ xsRead "/xenmgr/ui-ready" >>= return . (== Just "true")

_GetSafeGraphics :: Rpc Bool
_GetSafeGraphics = do
  cmdline <- liftIO $ readFile "/proc/cmdline"
  return $ case T.find (T.pack "safe-graphic") (T.pack cmdline) of
             (_, []) -> False
             _       -> True

-- This is called when UI notifies us about being ready
-- we fill in notification mvar and a xenstore node
_SetUiReady :: Bool -> Rpc ()
_SetUiReady value = do
    when value $ do
      already <- (== Just "true") <$> liftIO (xsRead "/xenmgr/ui-ready")
      when (not already) $ do
        focused <- inputGetFocusedDomainID
        when (focused == Nothing) $ switchGraphicsFallback >> return ()
        liftIO $ do
          uptime <- spawnShell "cat /proc/uptime"
          debug $ "received ui-ready notification: " ++ uptime
          xsWrite "/xenmgr/ui-ready" $ if value then "true" else "false"

_GetBuildInfo = do
    bi <- liftIO readBuildInfo
    edition <- liftIO appGetPlatformFlavour
    let m = M.fromList [ ("version", biVersion bi)
                       , ("build", biBuildNum bi)
                       , ("branch", biBuildBranch bi)
                       , ("build_date_time", biBuildDate bi)
                       , ("tools", biTools bi)
                       , ("release", biRelease bi) 
                       , ("edition", edition)]
    return m

-- List iso images in our iso folder
_ListIso :: Rpc [FilePath]
_ListIso = do
    d     <- appIsoPath
    files <- liftIO $ filesInDir d
    return $ map takeFileName . filter isGood $ files
  where
    isGood  x = isIso x && nonNull x
    nonNull x = (takeFileName x)  /= "null.iso"
    isIso   x = (takeExtension x) == ".iso"

_ListPciDevices :: Rpc [Map String String]
_ListPciDevices =
    do devices <- liftIO pciGetDevices
       mapM device_info devices
    where
      device_info d = do
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

_ListGpuDevices :: Rpc [Map String String]
_ListGpuDevices =
    map dict_entry <$> getGpuPlacements
  where
    dict_entry (d,p) =
      M.fromList $ [ ("addr", gpuId d)
                   , ("name", gpuName d)
                   , ("placement", show p) ]

_ListGpusForDisplayhandler :: Rpc [Map String String]
_ListGpusForDisplayhandler =
    map dict_entry <$> getGpusForDisplayhandler
  where
    dict_entry (d,c) =
      M.fromList $ [ ("addr", gpuId d)
                   , ("name", gpuName d)
                   , ("drmcard", show c) ]

_GetGpuPlacement :: String -> Rpc Int32
_GetGpuPlacement id = fromIntegral <$> getGpuPlacement id

_ConfigureGpuPlacement :: String -> Int32 -> Rpc ()
_ConfigureGpuPlacement id placement = configureGpuPlacement id (fromIntegral placement) >> updateSeamlessMouseSettings

_ListDiskDevices :: Rpc [Map String String]
_ListDiskDevices =
    do devs <- liftIO getHostStorageDevices
       return . map fromStorageDev $ devs
    where
      fromStorageDev d = M.fromList [ ("udi", storageUDI d)
                                    , ("block-device", storageBlockDevPath d) ]

pluginDir = "/usr/lib/xui/plugins"

_ListUiPlugins :: FilePath -> Rpc [String]
_ListUiPlugins dir = liftIO $ do 
       exists <- doesDirectoryExist (pluginDir </> dir)
       if not exists
          then return []
          else do     
            abs_dir <- canonicalizePath (pluginDir </> dir)
            if pluginDir `isPrefixOf` abs_dir
              then entries <$> getDirectoryContents abs_dir
              else error "bad subdirectory"
  where
    entries = filter (not . (`elem` [".", ".."]))

_GetSecondsFromEpoch :: Rpc Int32
_GetSecondsFromEpoch = round . realToFrac <$> liftIO epochTime

_GetInstallstate = do
  s <- liftIO $ getInstallState
  return $
         M.fromList
              [ ("deferred-dom0-password", show $ installDeferredDom0Password s)
              , ("deferred-kb-layout", show $ installDeferredKBLayout s)
              , ("deferred-accept-eula", show $ installDeferredAcceptEULA s)
              , ("deferred-language", show $ installDeferredLanguage s) ]

_ProgressInstallstate actionStr = do
  let action = fromString actionStr
  liftIO $ progressInstallState action

_ListCdDevices :: Rpc [Map String String]
_ListCdDevices = mapM info =<< liftIO getHostBSGDevices where
  info dev@(BSGDevice a b c d) = do
    let id = printf "%d:%d:%d:%d" a b c d
    (sticky,vm) <- _GetCdDeviceAssignment id
    usbid <- liftIO $ getUsbId dev
    liftIO $ do
      name <- getName id
      return $
        M.fromList $
        [
            ("id", id)
          , ("name", name)
          , ("vm", vm)
          , ("vm-sticky", if sticky then "1" else "0")
          , ("usb-id", fromMaybe "" (fmap show usbid))
        ]

  devp id = "/sys/class/bsg/" ++ id ++ "/device"
  getName id = do
    model  <- readNode $ devp id ++ "/model"
    vendor <- readNode $ devp id ++ "/vendor"
    return $ vendor ++ " " ++ model
  readNode n = do  
    e <- doesFileExist n
    if e then (strip <$> readFile n) else return ""
  getUsbId dev = fmap toID <$> getBSGDeviceUSBLocation dev where
    toID (b,d) = ((b.&.0xFF) `shiftL` 8) + d.&.0xFF

_GetCdDeviceAssignment :: String -> Rpc (Bool,String)
_GetCdDeviceAssignment idStr = case parseBSGDeviceId idStr of
  Nothing  -> error "bad device id"
  Just dev -> from <$> getCdDeviceVms dev
  where
    from ((uuid,sticky):_) = (sticky, uuidStr uuid)
    from _ = (False, "")

_AssignCdDevice :: String -> Bool -> String -> Rpc ()
_AssignCdDevice devidStr sticky uuidStr = case parseBSGDeviceId devidStr of
  Nothing  -> error "bad device id"
  Just dev -> do
    unassignCdDevice dev
    unassignStickyCdDevice dev
    when (not . null $ uuidStr) $
      if sticky
         then assignStickyCdDevice dev uuid
         else assignCdDevice dev uuid
    -- update autolock nodes as the desired value might've changed after
    -- modifying sticky bits
    al <- appGetAutolockCdDrives
    mapM_ (\uuid -> setVmAutolockCdDrives uuid al) =<< (filterM isRunning =<< getVms)
  where
    uuid = fromString uuidStr

_EjectCdDevice :: String -> Rpc ()
_EjectCdDevice devidStr = case parseBSGDeviceId devidStr of
  Nothing  -> error "bad device id"
  Just dev -> liftIO $ ejectCdDevice dev

updateSeamlessMouseSettings :: Rpc ()
updateSeamlessMouseSettings = mapM_ inputUpdateSeamlessMouseSettings =<< getVms

matchG :: String -> String -> [String]
matchG s regex =
    let (_,_,_,grps) = s =~ regex :: (String,String,String,[String])
    in grps

listSoundCards = liftIO
                 $ map kv
                 . catMaybes
                 . map parse
                 . lines <$> readFile "/proc/asound/cards" where
  parse line = case line `matchG` "\\s*(\\w+)\\s+\\[.*\\]: (.+)" of
    [id,name] -> Just (id,name)
    _ -> Nothing
  kv (id, name) = M.fromList [("id", id), ("name", name)]

parseSSControl line =
 case line `matchG` "(.) '(.+)' (\\w+) (.*)" of
    [dir, name,typ,value] -> Just (name,dir,typ,value)
    _ -> Nothing

listSoundCardControls cardID = liftIO controls where
  controls =
      map kv
    . catMaybes
    . map parseSSControl
    . lines
    <$> readProcessOrDie "/usr/lib/xen/bin/audio_helper" ["-c", cardID] ""
  kv (name,dir,typ,value) =
    M.fromList [
        ("name", name)
      , ("direction", dir)
      , ("type", typ)
      , ("value", value)
      ]

getSoundCardControl cardID name = liftIO $
  get . parseSSControl <$> readProcessOrDie "/usr/lib/xen/bin/audio_helper" ["-c", cardID, "sget", name] "" where
    get (Just (_,_,_,v)) = v
    get _ = ""

setSoundCardControl cardID name value = liftIO $ do
  case value `matchG` "^([0-9]+%) (on|off)$" of
    [lev, en] -> void $ readProcessOrDie "/usr/lib/xen/bin/audio_helper" ["-c", cardID, "sset", name, lev, en] ""
    _ ->  void $ readProcessOrDie "/usr/lib/xen/bin/audio_helper" ["-c", cardID, "sset", name, value] ""
