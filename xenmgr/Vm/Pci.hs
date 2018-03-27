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

{-# LANGUAGE ScopedTypeVariables #-}

module Vm.Pci (
             PciPtRuleID
           , PciPtRuleMap
           , PciInfo (..)
           , PciDriver (..)
           , pciFromStr
           , pciAndSlotFromStr
           , pciToStr
           , pciAndSlotToStr
           , pciUnbind
           , amtPciPtRules
           , pvmPciPtRules
           , gpuPciPtRule
           , matchingPciAddresses
           , matchingPciAddressesMany
           , pciAddRule
           , pciDelRule
           , pciGetDevice
           , pciGetDevices
           , pciGetDevicesFromAddrs
           , pciGetMatchingDevices
           , pciGetGpus
           , pciGetSecondaryGpus
           , pciMatchClass
           , pciSysfsDevPath
           , pciGetInfo
           , pciBindPciback
           , pciGetMMIOResources
           , pciGetMemHole
           , pciGetMemHoleBase
           , querySurfmanVgpuMode
           ) where

import Data.Char
import Data.Bits
import Data.Function
import Data.List
import Data.Maybe
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Unsafe
import Control.Concurrent
import Tools.Log

import Control.Monad
import Control.Monad.Error (catchError)
import Control.Applicative
import qualified Control.Exception as E
import Directory
import System.FilePath.Posix
import System.Posix.IO
import System.Posix.Files
import System.IO
import Text.Printf
import Text.Regex.Posix

import Tools.File
import Tools.Process
import Tools.Text
import Tools.Misc

import XenMgr.Rpc
import XenMgr.Db
import Rpc.Autogen.SurfmanClient

import Vm.PciDatabase
import Vm.Types

type PciPtRuleID  = Int
type PciPtRuleMap = M.Map PciPtRuleID PciPtRule
type ForceSlot    = Bool

data PciInfo = PciInfo { pciinfoVendor :: !Int
                       , pciinfoDevice :: !Int
                       , pciinfoClass :: !Int
                       , pciinfoBootVga :: !Bool
             } deriving (Eq, Show)

-- We can marshall a definition of PCI passthrough device
instance Marshall PciPtRule where
    dbRead x =
      do bdf <- dbMaybeRead (x ++ "/bdf")
         case bdf of
           Just bdf ->
             return $ fromMaybe (error "error in PCI BDF address") $
               do (addr,slot) <- pciAndSlotFromStr bdf
                  PciPtRuleBDF <$> pure addr <*> pure slot
           Nothing ->
             PciPtRule <$> dbMaybeRead (x ++ "/class")
                       <*> dbMaybeRead (x ++ "/vendor")
                       <*> dbMaybeRead (x ++ "/device")
                       <*> dbReadWithDefault False (x ++ "/force-slot")

    dbWrite x (PciPtRuleBDF addr fslot) =
      dbWrite (x ++ "/bdf") (pciAndSlotToStr (addr,fslot))

    dbWrite x (PciPtRule cls vendor device fslot) =
        do case cls of
             Nothing  -> dbRm (x ++ "/class")
             Just cls -> dbWrite (x ++ "/class") (printf "0x%04x" cls :: String)
           case vendor of
             Nothing  -> dbRm (x ++ "/vendor")
             Just v   -> dbWrite (x ++ "/vendor") (printf "0x%04x" v :: String)
           case device of
             Nothing  -> dbRm (x ++ "/device")
             Just d   -> dbWrite (x ++ "/device") (printf "0x%04x" d :: String)
           case fslot of
             False    -> dbRm (x ++ "/force-slot")
             True     -> dbWrite (x ++ "/force-slot") True

emptyRule :: PciPtRule
emptyRule = PciPtRule Nothing Nothing Nothing False

pciToStr :: PciAddr -> String
pciToStr = show

pciFromStr :: String -> Maybe PciAddr
pciFromStr =
    addr . splitBy (not . isHexDigit)
  where
    addr [a,b,c,d] = let [a',b',c',d'] = map num [a,b,c,d] in
                     Just $ PciAddr a' b' c' d'
    addr _         = Nothing
    num = read . ("0x" ++)

pciAndSlotFromStr :: String -> Maybe (PciAddr, Maybe Int)
pciAndSlotFromStr
  = go . split '@' where
    go [ p ]   = (,) <$> pciFromStr p <*> pure Nothing
    go [ p,q ] = (,) <$> pciFromStr p <*> (Just <$> maybeRead ( "0x"++q ))
    go _       = Nothing

pciAndSlotToStr :: (PciAddr, Maybe Int) -> String
pciAndSlotToStr (addr,Nothing) = pciToStr addr
pciAndSlotToStr (addr,Just sl) = pciToStr addr ++ "@" ++ show sl

-- parse domain:bus:slot.func@guest_slot syntax
parsePciPtDev :: PciPtSource -> String -> Maybe PciPtDev
parsePciPtDev src str =
    case split '@' str of
      (x:xs) -> do
        addr <- pciFromStr x
        let dev = PciDev addr (T.pack "") (T.pack $ pciSysfsDevPath addr)
        case xs of
          []         -> return (PciPtDev dev PciSlotDontCare False src)
          [slot_str] -> read_int slot_str >>= \slot -> return $ PciPtDev dev (PciSlotUse slot) False src
          _          -> Nothing
      _ -> Nothing

    where
      read_int :: String -> Maybe Int
      read_int str =
          case reads str of
            ((v,_):_) -> Just v
            _ -> Nothing

-- AMT passthrough devices -> fixed list
amtPciPtRules :: [PciPtRule]
amtPciPtRules =
    [ PciPtRule (Just 0x0780) (Just 0x8086) Nothing True
    , PciPtRule (Just 0x0700) (Just 0x8086) Nothing False
    ]

-- PVM passthrough devices are stored in database
-- default if the key is not present, is to pass a device of class 0x300
-- TODO: Why do we have a hard-coded Intel (0x8086) in here?
pvmPciPtRules :: Rpc [PciPtRule]
pvmPciPtRules =
    map hackGfxDevice <$>
    dbReadWithDefault [PciPtRule (Just 0x300) (Just 0x8086) Nothing True]
                      "/xenmgr/pvm-pci-pt-rules"
    where
      -- Device of class 0x300 requires force slot, we substitute it here
      hackGfxDevice r@(PciPtRule (Just 0x300) _ _ _) = r { ruleForceSlot = True }
      -- Otherwise we don't bother
      hackGfxDevice other                = other

-- gpu devices
-- default if the key is not present, is to pass a device of class 0x300
gpuPciPtRule :: PciPtRule
gpuPciPtRule = PciPtRule (Just 0x300) Nothing Nothing True

-- Match PCI devices on this machine against passthrough rules, output a result in the form of pci addresses
matchingPciAddresses :: PciPtRule -> IO [(PciAddr, PciPtGuestSlot)]
matchingPciAddresses (PciPtRuleBDF addr fslot)
  = return [ (addr, maybe PciSlotDontCare PciSlotUse fslot) ]
matchingPciAddresses rule@(PciPtRule {})
  = map slapForce . select <$> readPciCache where
  select (PciCache cache) = filter predAll cache
  pred rulePart infoPart (_,info) =
    maybe (const True) (==) (rulePart rule) $ infoPart info
  predMask rulePart infoPart (_,info) =
    maybe (const True) (==) (rulePart rule) $ ((infoPart info) .&. 0xFF00)
  predAll entry = gpuClass entry &&
                  pred ruleVendor pciinfoVendor entry &&
                  pred ruleDevice pciinfoDevice entry
  slapForce (dev, _) = (devAddr dev, guest_slot)
  guest_slot = if ruleForceSlot rule then PciSlotMatchHost else PciSlotDontCare
  gpuClass entry = if (maybe (const False) (==) (ruleClass rule) $ 0x300) then 
    predMask ruleClass pciinfoClass entry else pred ruleClass pciinfoClass entry

matchingPciAddressesMany :: [PciPtRule] -> IO [(PciAddr, PciPtGuestSlot)]
matchingPciAddressesMany rules = unions <$> mapM matchingPciAddresses rules
  where unions = S.toList . S.unions . map S.fromList

-- Todo: Do we need to update the cache, when we unbind?
pciUnbind :: PciAddr -> IO ()
pciUnbind addr =
    mapM_ maybeUnbind =<< omitPciback <$> filesInDir pciSysfsDevDir
  where
    omitPciback      = filter (\e -> takeFileName e /= "pciback")
    pciStr           = pciToStr addr
    maybeUnbind path = do
        have <- doesFileExist $ path </> pciStr
        when have $
          writeFile (path </> "unbind") pciStr

newRuleID :: PciPtRuleMap -> Int
newRuleID rules =
    maxID (M.keys rules) + 1
  where
    maxID [] = 0
    maxID xs = maximum xs

filterAway :: (PciPtRule -> Bool) -> PciPtRuleMap -> PciPtRuleMap
filterAway pred rules =
    M.filter (not . pred) rules

pciAddRule :: PciPtRule -> PciPtRuleMap -> PciPtRuleMap
pciAddRule rule rules =
    M.insert (newRuleID rules) rule rules

pciDelRule :: PciPtRule -> PciPtRuleMap -> PciPtRuleMap
pciDelRule rule@(PciPtRule a b c _) rules =
    filterAway matches rules
  where
    matches (PciPtRule a' b' c' _) = a == a' && b == b' && c == c'
    matches _ = False
pciDelRule rule@(PciPtRuleBDF _ _) rules =
    filterAway (== rule) rules

pciMatchClass :: Int -> PciPtRule
pciMatchClass cls = emptyRule { ruleClass = Just cls }

pciGetDevice :: PciAddr -> IO PciDev
pciGetDevice addr = fst . head . filter byAddr . unPciCache <$> readPciCache
  where byAddr (dev,_) = devAddr dev == addr

pciGetDevices :: IO [PciDev]
pciGetDevices = (map fst . unPciCache) <$> readPciCache

pciGetDevicesFromAddrs :: [PciAddr] -> IO [PciDev]
pciGetDevicesFromAddrs = mapM pciGetDevice

pciGetMatchingDevices :: PciPtSource -> [PciPtRule] -> IO [PciPtDev]
pciGetMatchingDevices src rules = unions <$> mapM from_rule rules
    where
      from_rule r = do
        let mkdev (addr, guest_slot) =
              PciPtDev <$> pciGetDevice addr <*> pure guest_slot <*> pure False <*> pure src
        mapM mkdev =<< matchingPciAddresses r
      unions = S.toList . S.unions . map S.fromList

pciGetGpus :: IO [PciDev]
pciGetGpus = mapM (pciGetDevice . fst) =<< matchingPciAddresses gpuPciPtRule

pciGetSecondaryGpus :: IO [PciDev]
pciGetSecondaryGpus = filterM pciGpuIsSecondary =<< pciGetGpus

pciGpuIsSecondary :: PciDev -> IO Bool
pciGpuIsSecondary dev = headDef True . map (not . pciinfoBootVga . snd) . filter byAddr . unPciCache <$> readPciCache
  where byAddr (dev',_) = on (==) devAddr dev' dev
        headDef def = fromMaybe def . listToMaybe

formattedPciAddr :: PciAddr -> String
formattedPciAddr (PciAddr domain bus slot func) =
        printf "%04x:%02x:%02x.%x" domain bus slot func

pciSysfsDevDir :: FilePath
pciSysfsDevDir = "/sys/bus/pci/devices"

pciSysfsDevPath :: PciAddr -> FilePath
pciSysfsDevPath addr = pciSysfsDevDir </> formattedPciAddr addr

pciDriverPath :: PciAddr -> FilePath
pciDriverPath addr = pciSysfsDevPath addr </> "driver"

pciGetInfo :: PciDev -> IO (Maybe PciInfo)
pciGetInfo dev = listToMaybe . map snd . filter byDev . unPciCache <$> readPciCache
  where byDev (dev',_) = dev' == dev

-- query the surface manager for list of pt devices in passthrough mode / vgpu mode parameters
querySurfmanVgpuMode :: Rpc (Maybe VgpuMode)
querySurfmanVgpuMode = do
    r <- ( Just <$>
           comCitrixXenclientSurfmanVgpuMode "com.citrix.xenclient.surfman" "/"
         ) `catchError` (\_ -> return Nothing)
    case r of
      Nothing -> return Nothing
      Just (max_count, dev_name, msi_trans, bdfs) ->
          let devices = pci_pt max_count bdfs msi_trans (T.pack dev_name) in
          return . Just $ VgpuMode (fromIntegral max_count) dev_name msi_trans devices

    where
      -- fill the msi translate flag & dev name
      pci_pt 0 _    _   _    = []
      pci_pt _ []   _   _    = []
      pci_pt _ bdfs msi name = map (fill msi name) . catMaybes . map (parsePciPtDev SourceVendorPlugin) $ bdfs

      fill msi name d =
          d { pciPtMsiTranslate = msi
            , pciPtDevice = (pciPtDevice d) { devNameT = name } }

-- | driver bits
newtype PciDriver = PciDriver { pciDriverName :: String } deriving (Eq, Show)
pciback = PciDriver "pciback"

posixWriteFile :: FilePath -> String -> IO ()
posixWriteFile path contents =
    E.bracket (openFd path WriteOnly Nothing defaultFileFlags) closeFd (write contents)
    where
      write []  fd = return ()
      write buf fd = fdWrite fd buf >>= \written -> write (drop (fromIntegral written) buf) fd

pciGetDriver :: PciAddr -> IO (Maybe PciDriver)
pciGetDriver addr =
    do exist <- doesDirectoryExist (pciDriverPath addr)
       if exist
          then return . PciDriver . takeBaseName <$> readSymbolicLink (pciDriverPath addr)
          else return Nothing

pciBindDriver :: PciAddr -> PciDriver -> IO ()
pciBindDriver addr drv@(PciDriver drvname) =
    do current <- pciGetDriver addr
       if (current == Just drv)
          then return ()
          else do info $ printf "binding %s to %s" (formattedPciAddr addr) drvname
                  posixWriteFile bindpath (formattedPciAddr addr)
    where
      bindpath = printf "/sys/bus/pci/drivers/%s/bind" drvname

pciBindPciback :: PciAddr -> IO ()
pciBindPciback addr = attempted =<< (E.try $
    do drv <- pciGetDriver addr
       case drv of
         Nothing                 -> bind
         Just dr | dr == pciback -> info $ printf "device %s already bound to pciback" (formattedPciAddr addr)
         Just dr                 -> unbind >> bind)
    where
      newslot = "/sys/bus/pci/drivers/pciback/new_slot"
      unbind  = pciUnbindCurrentDriver addr
      bind    = posixWriteFile newslot (formattedPciAddr addr) >> pciBindDriver addr pciback
      attempted (Right _) = return ()
      attempted (Left ex) = warn $ printf "error %s trying to bind %s to pciback" (show (ex :: E.SomeException)) (formattedPciAddr addr)

pciUnbindCurrentDriver :: PciAddr -> IO ()
pciUnbindCurrentDriver addr = pciGetDriver addr >>= unbind where
    unbind Nothing = return ()
    unbind (Just (PciDriver drvname)) = do
        info $ printf "unbinding device %s from driver %s" (formattedPciAddr addr) drvname
        posixWriteFile unbindpath (formattedPciAddr addr)
        where unbindpath = printf "/sys/bus/pci/drivers/%s/unbind" drvname

newtype PciCache = PciCache [(PciDev, PciInfo)]
unPciCache (PciCache x) = x

getFile :: FilePath -> IO String
getFile path = BC.unpack <$> B.readFile path

initCache :: IO PciCache
initCache = PciCache <$> enumerateDevices
  where
    loadPciDatabase :: IO PciDatabase
    loadPciDatabase = do
        pciDatabase <- loadDefault
        case pciDatabase of
             Left _ ->
                 -- an unexpected error occurred while loading the PCI database.
                 return Vm.PciDatabase.empty
             Right pciDatabase ->
                 return pciDatabase

    readDevID :: FilePath -> IO (FilePath, Int, Int, PciAddr)
    readDevID devFile =
      do let path = pciSysfsDevDir </> devFile
         device <- fromMaybe (error "error parsing device id") . maybeRead <$> getFile ( path </> "device")
         vendor <- fromMaybe (error "error parsing vendor id") . maybeRead <$> getFile ( path </> "vendor")
         return (path, vendor, device, fromJust (pciFromStr devFile))

    ourHostDevices :: IO [(FilePath, Int, Int, PciAddr)]
    ourHostDevices =
      mapM readDevID =<< getDirectoryContents_nonDotted pciSysfsDevDir

    enumerateDevices :: IO [(PciDev, PciInfo)]
    enumerateDevices = do
        db <- loadPciDatabase
        devs <- ourHostDevices
        let entries = gatherPciDeviceEntries db (map make_id devs)
        mapM (mkDev entries) devs
        where
          make_id ( _, vendor_id, dev_id, _ )
            = ( fromIntegral vendor_id
              , fromIntegral dev_id )

    mkDev entries (path, vendor_id, device_id, addr) = do
      cls <- (`div` 0x100) . fromMaybe (error "error parsing pci class") . maybeRead <$> getFile (path </> "class")
      bootVga <- (
          (=="1") . chomp <$> getFile (path </> "boot_vga")
        ) `E.catch` (\(_::E.IOException) -> return False)
      let dev =
            PciDev { devAddr = addr
                   , devNameT = findName entries (vendor_id, device_id)
                   , devSysfsPathT = T.pack (pciSysfsDevPath addr) }
          info =
            PciInfo { pciinfoClass = cls
                    , pciinfoDevice = fromIntegral device_id
                    , pciinfoVendor = fromIntegral vendor_id
                    , pciinfoBootVga = bootVga }
      return (dev, info)

    findName :: [DeviceEntry] -> (Int, Int) -> Text
    findName [] (vendor_id, dev_id) = T.pack $ printf "Unknown device (%04x:%04x)" vendor_id dev_id
    findName (e:es) id@(vendor_id, dev_id)
      | deId e == (fromIntegral vendor_id, fromIntegral dev_id)
      = T.concat [ TE.decodeUtf8 (deVendorName e)
                 , T.pack " "
                 , TE.decodeUtf8 (deDeviceName e)]
      | otherwise = findName es id

-- pciGetMMIOResources will parse PCI MMIO resources from the file at `path'.
-- It is written for sysfs PCI device `resource' file, (see
-- https://www.kernel.org/doc/Documentation/filesystems/sysfs-pci.txt,
-- e.g. /sys/bus/pci/devices/0000:00:02.0/resource).
-- `resource' file is formatted with 13 lines, each with 3 64bits hex values
-- separated by a space; with values meaning:
-- <PCI IO resource base address> <PCI IO resource end address> <resource flags>
-- (see linux/ioport.h for flags description).
pciGetMMIOResources :: String -> IO ([(Word, Word, Word)])
pciGetMMIOResources path = do
    content <- readFile path
    return $ resCat $ map resParse $ lines content
    where
        resParse line = case (words line) of
            [a, b, c] -> let [base, end, flags] = map (read::String->Word) [a, b, c] in
                         Just (base, end, flags)
            _ -> Nothing
        -- Keep only valid entries for IO-MEM.
        resCat res = [ (b, e, f) | Just (b, e, f) <- res,
                       b /= 0, e /= 0, (f .&. 0x00000200) /= 0 ]

pciGetMemHole :: [(Word, Word, Word)] -> Word
pciGetMemHole res =
    -- Defined by QEMU & XL as 0xf0000000-0xfbffffff:192M
    -- Default configuration from OpenXT's QEMU PCI devices will need ~64M.
    let dmMinHole = 0x4000000 in
    let resSizes = dmMinHole : [ e - b + 1 | (b, e, _) <- res ] in
        sum resSizes

pciGetMemHoleBase :: Word -> Word
pciGetMemHoleBase size =
    -- Consider, at least, that the upper 256M of address space are reserved (SeaBIOS),
    let highestTOLUM = 0xf0000000 in
    -- and the PCI hole has an upper boundary,
    let pciHoleLimit = 0xfc000000 in
    -- Align the TOLUM base to have sizes on 2 multiples (see SeaBIOS behaviour)
    let bases = [ 0xf0000000 .&. (shiftL highestTOLUM x) | x <- [0..3] ] in
    -- and get the first base low enough to accomodate for the cumulated size.
    head [ b | b <- bases, (pciHoleLimit - b) > size ]

pciCache :: MVar PciCache
{-# NOINLINE pciCache #-}
pciCache = unsafePerformIO (newMVar =<< initCache)

readPciCache :: IO PciCache
readPciCache = readMVar pciCache
