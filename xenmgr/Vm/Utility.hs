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

module Vm.Utility ( withMountedDisk, copyFileFromDisk
                  , tapCreate
                  , tapCreateVhd
                  , tapDestroy
                  , PartitionNum
                  , manageFrontVifs) where

import qualified Control.Exception as E
import Control.Applicative
import Control.Monad
import Data.Int
import Data.List
import Data.Maybe
import System.IO
import System.IO.Error
import System.FilePath
import Text.Printf

import Directory
import Tools.Misc
import Tools.Process
import Tools.Text
import Tools.Log

import Vm.DmTypes
import Vm.Dm
import Vm.DomainCore
import Vm.Queries
import Vm.Types
import Vm.Uuid

import XenMgr.Connect.Xl as Xl
import XenMgr.Rpc

type PartitionNum = Int

finally' = flip E.finally

mount :: FilePath -> FilePath -> Bool -> IO ()
mount dev dir ro = void $ readProcessOrDie "mount" ["-o", opts, dev, dir] "" where
  opts = intercalate "," . filter (not.null) $ [ro_opt]
  ro_opt | ro = "ro"
         | otherwise = ""

umount :: FilePath -> IO ()
umount dir = void $ readProcessOrDie "umount" [dir] ""

--Updated syntax for new tap-ctl style
tapCreate ty extraEnv ro path = chomp <$> readProcessOrDieWithEnv extraEnv "tap-ctl" ( ["create"] ++ ["-a", ty++":"++path] ) ""
tapCreateVhd = tapCreate "vhd"
tapCreateVdi = tapCreate "vdi"
tapCreateAio = tapCreate "aio"

tapDestroy :: String -> IO ()
tapDestroy path = void $  readProcessOrDie "tap-ctl" ["destroy", "-d", path] ""

withTempDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withTempDirectory root_path action =
    do directory <- attempt 1
       action directory `E.finally` removeDirectory directory
  where
    attempt n =
        let name = templated_name n in
        ( do createDirectory name
             return name ) `E.catch` create_error n
    create_error :: Int -> IOError -> IO FilePath
    create_error n e | isAlreadyExistsError e = attempt (n+1)
                     | otherwise              = E.throw e
    templated_name magic_num = root_path </> printf "tempdir-%08d" magic_num

withMountedDisk :: [(String,String)] -> DiskType -> Bool -> FilePath -> Maybe PartitionNum -> (FilePath -> IO a) -> IO a
withMountedDisk _ QemuCopyOnWrite _ _ _ _ = error "qcow unsupported"
withMountedDisk extraEnv diskT ro phys_path part action
  = withTempDirectory "/tmp" $ \temp_dir ->
      do dev <- create_dev diskT
         finally' (destroy_dev diskT dev) $
           do lo <- locreate dev
              finally' (loremove lo) $
                do loop <- lopart lo part
                   settle part
                   mount loop temp_dir ro
                   finally' (umount temp_dir) $ action temp_dir
  where
    create_dev DiskImage = return phys_path
    create_dev PhysicalDevice = return phys_path
    create_dev VirtualHardDisk = tapCreateVhd extraEnv ro phys_path
    create_dev ExternalVdi = tapCreateVdi extraEnv ro phys_path
    create_dev Aio = tapCreateAio extraEnv ro phys_path
    create_dev _ = error "unsupported"

    destroy_dev t dev | t `elem` [VirtualHardDisk, ExternalVdi, Aio] = tapDestroy dev
    destroy_dev _ _ = return ()

    loremove dev = readProcessOrDie "losetup" ["--detach", dev] ""
    locreate dev = chomp <$> readProcessOrDie "losetup" (args ++ [dev]) ""

    args = ["--find", "--show" ] ++ partscan part
    partscan Nothing = []
    partscan (Just pnum) = ["--partscan"]

-- losetup returns without waiting for the the partition device nodes.
-- The kernel creates them fast enough, but they aren't necessarily
-- labeled by by udev, and mount can fail from the incorrect label.
settle :: Maybe PartitionNum -> IO ()
settle Nothing = return ()
settle (Just pmnum) = do
    void $ readProcessOrDie "udevadm" ["settle"] ""

lopart :: FilePath -> Maybe PartitionNum -> IO FilePath
lopart lo Nothing = return lo
lopart lo (Just pnum) = do
    ex <- doesFileExist path
    case ex of
      True -> return path
      _    -> error $ "partition " ++ show pnum ++ " not found in " ++ show lo
    where
      path = lo ++ "p" ++ show pnum

deslash ('/':xs) = xs
deslash xs = xs

copyFileFromDisk :: [(String, String)] -> DiskType -> Bool -> FilePath -> (Maybe PartitionNum,FilePath) -> FilePath -> IO ()
copyFileFromDisk extraEnv diskT ro phys_path (part,src_path) dst_path
  = withMountedDisk extraEnv diskT ro phys_path part $ \contents ->
      void $ 
        readProcessOrDie "cp" [contents </> deslash src_path, dst_path] "" >>
          readProcessOrDie "sync" [] "" >> verifyChecksum (contents </> deslash src_path) dst_path
    where
    verifyChecksum src dst = do 
        src_sha_raw <- readProcessOrDie "sha256sum" [src] ""
        dst_sha_raw <- readProcessOrDie "sha256sum" [dst] ""
        let src_sha = head $ split ' ' $ src_sha_raw
        let dst_sha = head $ split ' ' $ dst_sha_raw
        info $ "copyFile src_sha: " ++ src_sha
        info $ "copyFile dst_sha: " ++ dst_sha
        if src_sha /= dst_sha
          then error "copy file shasums dont match!"
          else return ()


manageFrontVifs :: Bool -> Uuid -> Rpc ()
manageFrontVifs connect_action back_uuid =
    do
       vms  <- filter ((/=) back_uuid) <$> (filterM Xl.isRunning =<< getVms)
       devs <- mapM getdevs vms
       devices <- filterM (uses back_uuid) (concat devs)
       mapM_ (manage connect_action) devices

    where
      getdevs :: Uuid -> Rpc [(Uuid, DmFront)]
      getdevs uuid = whenDomainID [] uuid $ \domid -> do
        -- TODO: only supporting vif,vwif devices atm
        vifs  <- liftIO $ getFrontDevices VIF  domid
        vwifs <- liftIO $ getFrontDevices VWIF domid
        return $ zip (repeat uuid) (vifs ++ vwifs)
      uses bkuuid (_,d) = do 
            domid <- liftIO $ Xl.getDomainId bkuuid
            if domid /= "" then return $ (read domid :: DomainID) == dmfBackDomid d else return False
      manage connect_action (front_uuid, dev) = do
          let nid@(XbDeviceID nic_id) = dmfID dev
          liftIO $ Xl.connectVif front_uuid nid connect_action

