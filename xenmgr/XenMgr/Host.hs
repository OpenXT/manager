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

{-# LANGUAGE OverloadedStrings, PatternGuards, ScopedTypeVariables, CPP #-}
module XenMgr.Host (
              getTotalMem
            , getFreeMem
            , getCpuCount
            , getTotalStorage
            , getFreeStorage
            , getHvmInfo
            , getHostState
            , setHostState
            , getHostStorageDevices
            , getHostBSGDevices
            , getBSGDeviceUSBLocation
            , getHostSystemManufacturer, getHostSystemManufacturer', getHostSystemProductName
            , getHostSystemVersion, getHostBiosVersion, getHostChassisType
            , getHostSystemSerialNumber
            , getHostXcVersion
            , getHostPlaybackDevices
            , getHostCaptureDevices
            , getSelectedHostPlaybackDevice
            , getSelectedHostCaptureDevice
            , selectHostPlaybackDevice
            , selectHostCaptureDevice
            , parseBSGDeviceId
            , bsgDeviceIdStr
            , StorageDevice (..), BSGDevice (..)
            , PcmDeviceId (..)
            , PcmDevice (..)
            , HostState (..), HostInfo (..), CachedHostInfo, HvmInfo (..)
            , Manufacturer (..)
            , eth0Mac, getEth0Model
            , wlan0Mac, getWlan0Model
            , haveWireless
            , haveSystemAmtPt
            , initHostInfo
            , getHostInfo
            , hostCheckFreeStorage
            , isServiceRunning
            , isUpdatePending

            , getEULA
            , getInstallState
            , progressInstallState
            , isInstallCompleted
            , InstallState (..), InstallAction (..)
            , HostGpu (..)
            , getHostGpus, getGpuPlacement, getGpuPlacements, configureGpuPlacement
            , getGpusLeftOf, getGpusRightOf

            , pageSize
            , pagesToMib
            , pagesToKib
            , mibToKib
            , kibToMib
            , PhysInfo (..)
            , getPhysInfo

            , ExtPack (..)
            , getExtPacks

            , hostIsLicensed
            , hostSetLicense
            , hostLicenseInit
            ) where

import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Locale
import Text.Printf
import qualified Data.Map as M
import Data.String
import qualified Data.Text.Lazy as T
import Control.Concurrent
import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Applicative
import Control.DeepSeq
import qualified Control.Exception as E
import System.Posix.Files
import System.Process
import System.IO
import System.IO.Unsafe
import System.FilePath
import Directory
import Text.Regex.Posix

import Tools.Log
import Tools.File
import Tools.Misc
import Tools.XenStore
import Tools.Text
import Tools.Process
import Tools.Future

import XenMgr.Rpc
import XenMgr.Config
import XenMgr.Errors
import Vm.Types
import Vm.Pci
import XenMgr.Db
import XenMgr.Notify

-- At what % do we consider storage partition to be critically low and start sending notifications
storageSpaceLowThreshold = 5

data HostState = HostIdle
               | HostShuttingDown
               | HostRebooting
               | HostGoingToSleep
               | HostGoingToHibernate
                 deriving (Eq, Show)

data HostInfo = HostInfo {
      hostVendor :: String
    , hostModel :: String
    , hostSerial :: String
    , hostBiosRev :: String
    , hostPhysCpuModel :: String
    , hostPhysGpuModel :: String
    , hostEth0Mac :: String
    , hostEth0Model :: String
    , hostWifiMac :: String
    , hostWifiModel :: String
    , hostChassisType :: String
    , hostIsLaptop :: Bool
    , hostIsAmtCapable :: Bool
    }

data CachedHostInfo = CachedHostInfo (MVar HostInfo) (MVar Bool)

data StorageDevice = StorageDevice {
      storageBlockDevPath :: FilePath
    , storageUDI :: String }

data BSGDevice = BSGDevice {
      bsgScsi :: Int
    , bsgChannel :: Int
    , bsgId :: Int
    , bsgLun :: Int } deriving (Eq,Ord)

data HvmInfo = HvmInfo {
      hvmEnabled :: Bool
    , hvmDirectIOEnabled :: Bool
}

newtype PcmDeviceId = PcmDeviceId { pcmDeviceIdStr :: String }
data PcmDevice = PcmDevice {
      pcmId :: PcmDeviceId
    , pcmName :: String
    , pcmPlayback :: Bool
    , pcmCapture :: Bool
}

data Manufacturer = DELL | OtherManufacturer String

data ExtPack
   = ExtPack { extpackId :: String
             , extpackVendor :: String
             , extpackName :: String
             , extpackDescr :: String
             , extpackVersion :: String
             , extpackImage :: String
             } deriving (Eq, Show)

instance Marshall ExtPack where
  dbRead x =
    ExtPack <$> pure id <*> s "vendor" <*> s "name" <*> s "description" <*> s "version" <*> s "image"
      where s tag = dbRead (x ++ "/" ++ tag)
            id = shead . reverse $ dbPathSplit x
            shead (v:_) = v
            shead [] = error "extpack: unexpected empty list"

  dbWrite x v = error "extpack: write unsupported"

getExtPacks :: Rpc [ExtPack]
getExtPacks = dbRead "/extpack"

getHostState :: MonadRpc e m => m HostState
getHostState =
    liftIO $ xsRead "/xenmgr/host-state" >>= return . unstr
    where unstr (Just "idle") = HostIdle
          unstr (Just "shutting-down") = HostShuttingDown
          unstr (Just "rebooting") = HostRebooting
          unstr (Just "going-to-sleep") = HostGoingToSleep
          unstr (Just "going-to-hibernate") = HostGoingToHibernate
          unstr _ = HostIdle

setHostState :: MonadRpc e m => HostState -> m ()
setHostState s =
    do liftIO $ xsWrite "/xenmgr/host-state" (str s)
       notifyHostStateChanged (uistr s)
    where str HostIdle = "idle"
          str HostShuttingDown = "shutting-down"
          str HostRebooting = "rebooting"
          str HostGoingToSleep = "going-to-sleep"
          str HostGoingToHibernate = "going-to-hibernate"
          -- UI expects this in different str format, for now
          uistr HostIdle = "idle"
          uistr HostShuttingDown = "shutdowning"
          uistr HostRebooting = "rebooting"
          uistr HostGoingToSleep = "sleeping"
          uistr HostGoingToHibernate = "hibernating"

getHostStorageDevices :: IO [StorageDevice]
getHostStorageDevices =
    do udis <- lines <$> safeSpawnShell "hal-find-by-property --key info.category --string storage"
       mapM fromUDI udis
    where
      fromUDI udi =
          do block_path <- chomp <$> safeSpawnShell ("hal-get-property --udi " ++ udi ++ " --key block.device")
             return StorageDevice { storageBlockDevPath = block_path
                                  , storageUDI = udi }

bsgDeviceIdStr :: BSGDevice -> String
bsgDeviceIdStr (BSGDevice a b c d) = printf "%d:%d:%d:%d" a b c d

parseBSGDeviceId :: String -> Maybe BSGDevice
parseBSGDeviceId s = case split ':' s of
  [a,b,c,d] -> BSGDevice <$> p a <*> p b <*> p c <*> p d
  _ -> Nothing
  where
    p s = case reads s of
      [(num,_)] -> Just num
      _ -> Nothing

getHostBSGDevices :: IO [BSGDevice]
getHostBSGDevices =
    do ls <- lines <$> spawnShell "grep -B2 CD-ROM /proc/scsi/scsi | grep Host"
       return . catMaybes $ map dev ls
    where
      dev l =
          case l `matchG` "^.*scsi([0-9]+).* ([0-9]+) .* ([0-9]+) .* ([0-9]+)" of
            [ scsi, ch, id, lun ] -> Just $ BSGDevice (read scsi) (read ch) (read id) (read lun)
            _ -> Nothing

getBSGDeviceUSBLocation :: BSGDevice -> IO (Maybe (Int,Int))
getBSGDeviceUSBLocation (BSGDevice a b c d) = do
  path <- ("/sys/bus/scsi/devices" </>) <$> readSymbolicLink scsipath
  scan (splitPath path)
  where
    scsipath  = printf "/sys/bus/scsi/devices/%d:%d:%d:%d" a b c d
    scan [] = return Nothing
    scan p | last p == "../" = return Nothing
    scan p = do
      let p' = joinPath p
      ss <- subsystem p'
      case ss of
        Just "usb" -> scanusb p
        _ -> scan (parent p)
    scanusb :: [FilePath] -> IO (Maybe (Int,Int))
    scanusb p = do
      let p' = joinPath p
      bus <- join . fmap maybeRead <$> maybeGetContents (p' </> "busnum")
      dev <- join . fmap maybeRead <$> maybeGetContents (p' </> "devnum")
      case (bus,dev) of
        (Just b, Just d) -> return $ Just (b,d)
        _ -> scan (parent p)
    subsystem p = do
      (Just . takeFileName <$> readSymbolicLink (p </> "subsystem"))
      `E.catch` (\(e::E.SomeException) -> return Nothing)

    parent [] = []
    parent xs = take (n-1) xs where n = length xs
    last [] = error "empty list in last()"
    last xs = head (reverse xs)

maybeStrip :: Maybe String ->  Maybe String
maybeStrip (Just s) = Just $ strip s
maybeStrip Nothing  = Nothing

eth0Mac :: IO (Maybe Mac)
eth0Mac = maybeGetContents "/sys/class/net/eth0/address" >>= return . maybeStrip

wlan0Mac :: IO (Maybe Mac)
wlan0Mac = maybeGetContents "/sys/class/net/wlan0/address" >>= return . maybeStrip

getEth0Model :: IO String
getEth0Model = from =<< pciGetMatchingDevices SourceConfig [PciPtRule (Just 0x200) Nothing Nothing False] where
    from (d:_) = return . devName . pciPtDevice $ d
    from _ = return ""

getWlan0Model :: IO String
getWlan0Model = from =<< pciGetMatchingDevices SourceConfig [PciPtRule (Just 0x280) Nothing Nothing False] where
    from (d:_) = return . devName . pciPtDevice $ d
    from _ = return ""

-- 0x280 is the pci class for wifi devices
haveWireless :: IO Bool
haveWireless =
    do addrs <- matchingPciAddresses $ pciMatchClass 0x280
       return . not . null $ addrs

matchG :: String -> String -> [String]
matchG s regex =
    let (_,_,_,grps) = s =~ regex :: (String,String,String,[String])
    in grps

data PhysInfo
   = PhysInfo { cpuCount :: Int
              , totalPages :: Int
              , freePages :: Int
              , scrubPages :: Int }
     deriving ( Eq )

pagesToKib, pagesToMib, mibToKib,kibToMib :: (Integral a) => a -> a

pageSize :: Integral a => a
pageSize = 4096
pagesToMib = (`div` 1024) . pagesToKib
pagesToKib npages = npages * (pageSize `div` 1024)
mibToKib = (*1024)
kibToMib = (`div` 1024)

getPhysInfo :: IO PhysInfo
getPhysInfo = parse (PhysInfo 0 0 0 0) . lines <$> xenops where
  xenops = readProcessOrDie "xenops" ["physinfo"] ""
  parse pi [] = pi
  parse pi (l:ls) = case words l of
    ("nr_cpus":_:v:_)     -> parse (pi{ cpuCount = read v }) ls
    ("total_pages":_:v:_) -> parse (pi{ totalPages = read v }) ls
    ("free_pages" :_:v:_) -> parse (pi{ freePages = read v }) ls
    ("scrub_pages":_:v:_) -> parse (pi{ scrubPages = read v }) ls
    _                     -> parse pi ls

getTotalMem :: IO Int
getTotalMem = pagesToMib . totalPages <$> getPhysInfo

getFreeMem :: IO Int
getFreeMem = pagesToMib . freePages <$> getPhysInfo

getCpuCount :: IO Int
getCpuCount = cpuCount <$> getPhysInfo

-- in megabytes, total space + free space
diskSpaceInfo :: String -> IO (Int, Int)
diskSpaceInfo path =
    do size  <- fromIntegral <$> getBlockSize path
       total <- getTotalBlocks path
       avail <- getAvailBlocks path
       return ( fromIntegral ((total*size) `div` 1024 `div` 1024)
              , fromIntegral ((avail*size) `div` 1024 `div` 1024) )

getTotalStorage :: IO Int
getTotalStorage = diskSpaceInfo "/storage" >>= return . fst

getFreeStorage :: IO Int
getFreeStorage = diskSpaceInfo "/storage" >>= return . snd

getFreeStoragePercent :: IO Int
getFreeStoragePercent =
    do free  <- fromIntegral <$> getFreeStorage
       total <- fromIntegral <$> getTotalStorage
       return . round $ (free / total) * 100.0

hostCheckFreeStorage :: Rpc ()
hostCheckFreeStorage = do
    percent <- liftIO getFreeStoragePercent
    when (percent <= storageSpaceLowThreshold) $
         notifyStorageSpaceLow percent

-- Test if System Amt PT is active (by checking database)
haveSystemAmtPt :: Rpc Bool
haveSystemAmtPt = dbReadWithDefault False "system-amt-pt-active"

-- Check if this is laptop by testing the chassis type string
isLaptop :: IO Bool
isLaptop =
    do hi <- readHostInfo
       let chassis = map toLower . strip $ hostChassisType hi
       return (chassis == "laptop" || chassis == "notebook" || chassis == "portable")

-- Check if host contains PCI devices required to turn AMT on
isAMTCapable :: IO Bool
isAMTCapable =
    not . null <$> matchingPciAddressesMany amtPciPtRules

getHvmInfo :: IO HvmInfo
getHvmInfo = do
    text <- T.pack <$> spawnShell "xenops physinfo"
    let hvm = T.count "hvm" text > 0
        directio = T.count "directio" text > 0
    return $ HvmInfo { hvmEnabled = hvm
                     , hvmDirectIOEnabled = directio }

initHostInfo :: IO CachedHostInfo
initHostInfo = CachedHostInfo <$> newEmptyMVar <*> newMVar False

getHostInfo :: CachedHostInfo -> IO HostInfo
getHostInfo (CachedHostInfo hi initialised) = modifyMVar initialised $ \init ->
  do when (not init) $
       putMVar hi =<< readHostInfo
     (,) <$> pure True <*> readMVar hi

dmiDecodeKey :: String -> IO String
dmiDecodeKey key = chomp <$> readProcessOrDie "dmidecode" ["-s", key] ""

getHostSystemManufacturer :: IO String
getHostSystemManufacturer = dmiDecodeKey "system-manufacturer"

getHostSystemManufacturer' :: IO Manufacturer
getHostSystemManufacturer' = from_string . map toLower <$> getHostSystemManufacturer where
    from_string s | "dell" `isPrefixOf` s = DELL
                  | otherwise = OtherManufacturer s

getHostSystemProductName :: IO String
getHostSystemProductName = dmiDecodeKey "system-product-name"

getHostSystemSerialNumber :: IO String
getHostSystemSerialNumber = dmiDecodeKey "system-serial-number"

getHostSystemVersion :: IO String
getHostSystemVersion = dmiDecodeKey "system-version"

getHostChassisType :: IO String
getHostChassisType = dmiDecodeKey "chassis-type"

getHostBiosVersion :: IO String
getHostBiosVersion = dmiDecodeKey "bios-version"

readHostInfo :: IO HostInfo
readHostInfo = do
    vendor  <- future $ getHostSystemManufacturer
    model   <- future $ getHostSystemProductName
    serial  <- future $ getHostSystemSerialNumber
    biosRev <- future $ getHostBiosVersion
    -- interleave these forks for some extra performance during boot
    physCpuModel   <- future $
                        do out <- spawnShell "grep \"model name\" /proc/cpuinfo"
                           return $ (strip out) `fetch` "model name[^:]*:(.*)"
    physGpuModel   <- future $ strip <$> spawnShell "lspci  -nn | grep \"\\[0300\\]\" | cut -d: -f3-"
    eth0Model      <- future $ getEth0Model
    wifiModel      <- future $ getWlan0Model
    eth0Addr       <- future $ maybe "" id <$> eth0Mac
    wifiAddr       <- future $ maybe "" id <$> wlan0Mac
    amt_cap        <- future $ isAMTCapable
    chassisType <- getHostChassisType
    let chassis = map toLower . strip $ chassisType
        is_laptop = (chassis == "laptop" || chassis == "notebook" || chassis == "portable")
    force $ HostInfo <$>
          vendor
      <*> model
      <*> serial
      <*> biosRev
      <*> physCpuModel
      <*> physGpuModel
      <*> eth0Addr
      <*> eth0Model
      <*> wifiAddr
      <*> wifiModel
      <*> pure chassisType
      <*> pure is_laptop
      <*> amt_cap
    where
      fetch s regex = case s `matchG` regex of []    -> ""
                                               (x:_) -> strip x

----------------------------------------
-- xenclient installation progress/state
----------------------------------------

data InstallState = InstallState {
      installDeferredDom0Password :: Bool
    , installDeferredKBLayout :: Bool
    , installDeferredAcceptEULA :: Bool
    , installDeferredLanguage :: Bool
}
data InstallAction = Dom0PasswordSet | KBLayoutSet | EULAAccepted | LanguageSet deriving Eq

getEULA :: Rpc String
getEULA = do
    lang <- appGetLanguage
    let choose names
            = map fst . filter snd <$> mapM (\n -> liftIO (doesFileExist n) >>= return . (,) n) names

    eulas <- choose [ "/usr/share/xenclient/EULA-" ++ lang
                    , "/usr/share/xenclient/EULA-en-us"
                    , "/usr/share/xenclient/EULA" ]
    case eulas of
      []       -> return ""
      (eula:_) -> do
                 liftIO . withFile eula ReadMode $ \handle -> do
                    hSetEncoding handle utf8
                    contents <- hGetContents handle
                    contents `deepseq` return contents

getInstallState :: IO InstallState
getInstallState = do
  dom0Pass <- doesFileExist "/config/deferred_dom0_password"
  kbLayout <- doesFileExist "/config/deferred_kb_layout"
  eula     <- doesFileExist "/config/deferred_eula"
  lang     <- doesFileExist "/config/deferred_language"
  return $ InstallState { installDeferredDom0Password = dom0Pass
                        , installDeferredKBLayout = kbLayout
                        , installDeferredAcceptEULA = eula
                        , installDeferredLanguage = lang }

progressInstallState :: InstallAction -> IO ()
progressInstallState Dom0PasswordSet = removeExistingFile "/config/deferred_dom0_password"
progressInstallState KBLayoutSet     = removeExistingFile "/config/deferred_kb_layout"
progressInstallState EULAAccepted    = removeExistingFile "/config/deferred_eula"
progressInstallState LanguageSet     = removeExistingFile "/config/deferred_language"

isInstallCompleted :: IO Bool
isInstallCompleted =
    getInstallState >>= return . completed
  where
    completed (InstallState False False False False ) = True
    completed _                                       = False

removeExistingFile :: FilePath -> IO ()
removeExistingFile p = doesFileExist p >>= maybeRemove
    where maybeRemove True = removeFile p
          maybeRemove _    = return ()

-- mapping from actions to strings
installActionStrs =
    [ (Dom0PasswordSet, "dom0-password-set")
    , (KBLayoutSet,     "kb-layout-set")
    , (EULAAccepted,    "eula-accepted")
    , (LanguageSet,     "language-set") ]

installActionStrsRev =
    [ (b,a) | (a,b) <- installActionStrs ]

instance IsString InstallAction where
    fromString s =
        case lookup s installActionStrsRev of
          Just action -> action
          _           -> error $ "unexpected install-action string: " ++ s

instance Show InstallAction where
    show a = let Just str = lookup a installActionStrs in str


isServiceRunning :: String -> Rpc Bool
isServiceRunning name =
    --TODO: add service specific check for exported objects
    serviceNameTaken name

isUpdatePending :: Rpc Bool
isUpdatePending = dbExists "/updatemgr/update-state"

data HostGpu =
     HostGpu { gpuId :: String
             , gpuName :: String } deriving (Eq,Show)

getSurfmanGpu :: Rpc (Maybe HostGpu)
getSurfmanGpu = do
    devices  <- liftIO pciGetDevices
    devMatch <- filterM boot_vga_filter devices
    case devMatch of
        [] -> return Nothing
        _  -> return $ Just $ HostGpu "hdx" (show (devName (head devMatch)))

    where
        boot_vga_filter d = do
            let boot_vga_file = devSysfsPath d </> "boot_vga"
            boot_vga_exists <- liftIO $ doesFileExist boot_vga_file
            if boot_vga_exists 
                then do contents <- chomp <$> (liftIO $ readFile boot_vga_file)
                        case contents of
                            "1" -> return True
                            _   -> return False
                else return False

hostGpus :: MVar (Maybe [HostGpu])
{-# NOINLINE hostGpus #-}
hostGpus = unsafePerformIO (newMVar Nothing)

getHostGpus :: Rpc [HostGpu]
getHostGpus =
  do c <- rpcGetContext
     liftIO $ modifyMVar hostGpus (f c)
  where
    f c v@(Just gpus) = return (v, gpus)
    f c Nothing = handle =<< rpc c getHostGpus'
      where handle (Left ex)    = E.throw ex
            handle (Right gpus) = return (Just gpus, gpus)

getHostGpus' :: Rpc [HostGpu]
getHostGpus' = do
  surfman   <- to_l <$> getSurfmanGpu
  secondary <- secondary_devs =<< appMultiGpuPt
  return ( surfman ++ map host_gpu secondary )
  where
    to_l Nothing  = []
    to_l (Just x) = [x]
    host_gpu d = HostGpu (show $ devAddr d) (devName d)
    -- HACK: if multi gpu pt is disabled, we don't return secondary devices
    secondary_devs allow_multi
        | allow_multi = liftIO pciGetSecondaryGpus
        | otherwise   = return []

getGpuPlacement :: String -> Rpc Int
getGpuPlacement gpu_id
  = do assertGpuId gpu_id
       place <- dbMaybeRead ("/xenmgr/gpu-placement/" ++ gpu_id)
       case place of
         Just p -> return p
         Nothing -> do
           gpus <- map gpuId <$> getHostGpus
           return . fromMaybe 0 . lookup gpu_id $ zip gpus [0..]

assertGpuId :: String -> Rpc ()
assertGpuId gpu_id =
    do contains <- (gpu_id `elem`) . map gpuId <$> getHostGpus
       when (not contains) . failUnknownGpu $ gpu_id

configureGpuPlacement :: String -> Int -> Rpc ()
configureGpuPlacement gpu_id n = assertGpuId gpu_id >> dbWrite ("/xenmgr/gpu-placement/" ++ gpu_id) n

getGpuPlacements :: Rpc [(HostGpu,Int)]
getGpuPlacements = getHostGpus >>= \gpus -> zip gpus <$> mapM placement gpus
    where placement gpu = getGpuPlacement (gpuId gpu)

getGpusLeftOf :: String -> Rpc [HostGpu]
getGpusLeftOf gpu_id =
    do p <- getGpuPlacement gpu_id
       map fst <$> filter (left_of p) . reverse . sortBy (comparing snd) <$> getGpuPlacements
    where left_of p (gpu,p') = p' < p

getGpusRightOf :: String -> Rpc [HostGpu]
getGpusRightOf gpu_id =
    do p <- getGpuPlacement gpu_id
       map fst <$> filter (right_of p) . sortBy (comparing snd) <$> getGpuPlacements
    where right_of p (gpu,p') = p' > p

getHostXcVersion :: Rpc XcVersion
getHostXcVersion = XcVersion . version . dictFile <$> liftIO (readFile "/etc/xenclient.conf") where
    version m = fromMaybe "" . M.lookup "version" $ m

dictFile :: String -> M.Map String String
dictFile =
    M.fromList
           . catMaybes
           . map (safeHead2 . split '=')
           . lines
    where
      safeHead2 (k:v:_) = Just (strip k,strip v)
      safeHead2 _ = Nothing

parseExpiryDate :: String -> Maybe UTCTime
parseExpiryDate dateStr = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" dateStr

hostLicenseInit :: IO ()
hostLicenseInit = xsWrite "/xenmgr/license/hash" "local"

hostIsLicensed :: IO Bool
hostIsLicensed = do
  expiry <- fromMaybe "" <$> xsRead "/xenmgr/license/expiry"
  deviceUuid <- fromMaybe "" <$> xsRead "/xenmgr/license/deviceuuid"
  hash <- fromMaybe "" <$> xsRead "/xenmgr/license/hash"
  time <- getCurrentTime
  case (hash, parseExpiryDate expiry) of
    ("local", _) -> return True
    (_, Nothing) -> return False
    (_, Just expiryDate) -> do
      let input = printf "%s%s%s" deviceUuid magic expiry
      hash2 <- calcHash input
      case () of
        _ | not (null hash), hash == hash2, time <= expiryDate -> return True
          | otherwise -> return False
  where
   magic :: String
   magic = "Copyright (c) 2012 Citrix Systems, Inc.- XenClient - {3282892a-f098-4e3d-a591-9504b704a4d1}"
   calcHash i =
     head . words <$> readProcessOrDie "sha256sum" [] i    

hostSetLicense :: String -> String -> String -> Rpc ()
hostSetLicense _ _ "local" = error "invalid hash"
hostSetLicense expiryDateStr deviceUuid hashStr = do
  before <- liftIO hostIsLicensed
  case parseExpiryDate expiryDateStr of
    Nothing -> error "failed to parse expiry date"
    Just expiryDate -> do
      liftIO $ do
        xsWrite "/xenmgr/license/expiry" expiryDateStr
        xsWrite "/xenmgr/license/deviceuuid" deviceUuid
        xsWrite "/xenmgr/license/hash" hashStr
      after <- liftIO hostIsLicensed
      when (before /= after) $ notifyLicenseChanged

getHostPcmDevices :: IO [PcmDevice]
getHostPcmDevices = catMaybes . map parseDev . lines <$> readFile "/proc/asound/pcm" where
  parseDev l = case map strip (split ':' l) of
    (idStr : name : _ : rest) ->
      Just $ PcmDevice
               (PcmDeviceId idStr)
               name
               ("playback 1" `elem` rest)
               ("capture 1" `elem` rest)
    _ -> Nothing

getHostPlaybackDevices = filter pcmPlayback <$> getHostPcmDevices
getHostCaptureDevices = filter pcmCapture <$> getHostPcmDevices

getSelectedHostPlaybackDevice :: Rpc PcmDeviceId
getSelectedHostPlaybackDevice = PcmDeviceId . fromMaybe "00-00" <$> dbMaybeRead "/audio/playback-pcm"

getSelectedHostCaptureDevice :: Rpc PcmDeviceId
getSelectedHostCaptureDevice = PcmDeviceId . fromMaybe "00-00" <$> dbMaybeRead "/audio/capture-pcm"

selectHostPlaybackDevice :: PcmDeviceId -> Rpc ()
selectHostPlaybackDevice (PcmDeviceId idS) = do
  dbWrite "/audio/playback-pcm" idS
  liftIO . void $ readProcessOrDie "update-pcm-config" [] ""

selectHostCaptureDevice :: PcmDeviceId -> Rpc ()
selectHostCaptureDevice (PcmDeviceId idS) = do
  dbWrite "/audio/capture-pcm" idS
  liftIO . void $ readProcessOrDie "update-pcm-config" [] ""
