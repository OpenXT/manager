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
                  , PartitionNum ) where

import qualified Control.Exception as E
import Control.Applicative
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
import Tools.PartTable

import Vm.DmTypes

type PartitionNum = Int

finally' = flip E.finally

mount :: FilePath -> FilePath -> Int64 -> Bool -> Bool -> IO ()
mount dev dir off loop ro = void $ readProcessOrDie "mount" ["-o", opts, dev, dir] "" where
  opts = intercalate "," . filter (not.null) $ [ro_opt,off_opt,loop_opt]
  ro_opt | ro = "ro"
         | otherwise = ""
  off_opt | off == 0 = ""
          | otherwise = "offset=" ++ show off
  loop_opt | not loop = ""
           | otherwise = "loop"

umount :: FilePath -> IO ()
umount dir = void $ readProcessOrDie "umount" [dir] ""

--Updated syntax for new tap-ctl style
tapCreate ty extraEnv ro path = chomp <$> readProcessOrDieWithEnv extraEnv "tap-ctl" ( ["create"] ++ ["-a", ty++":"++path] ) ""
tapCreateVhd = tapCreate "vhd"
tapCreateVdi = tapCreate "vdi"
tapCreateAio = tapCreate "aio"

--tapDestroy syntax changed as well, requires more information than just the tap device
--So, we provide the pid and minor number of the tapped instance.
tapDestroy :: String -> IO ()
tapDestroy path =
    do
        tapInfo <- readProcess "tap-ctl" ["list", "-f", path] ""
        destroyTap (words tapInfo)
    where
        destroyTap info =
          case info of
            [pid, minor, _, typ, p] -> void $ readProcessOrDie "tap-ctl" ["destroy","-p",pid,"-m",minor] ""
            _ -> return ()

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
           do off <- mountOffset dev part
              mount dev temp_dir off (loop diskT) ro
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

    loop DiskImage = True
    loop _ = False

mountOffset :: FilePath -> Maybe PartitionNum -> IO Int64
mountOffset dev Nothing = return 0
mountOffset dev (Just pnum) =
  do ptable <- fromMaybe (error $ "failed to read parition table of " ++ dev) <$> readPartTable dev
     case filter (\p -> partNum p == pnum) ptable of
       [] -> error $ "partition " ++ show pnum ++ " not found in " ++ show dev
       (x:_) -> return $ partStart x

deslash ('/':xs) = xs
deslash xs = xs

copyFileFromDisk :: [(String, String)] -> DiskType -> Bool -> FilePath -> (Maybe PartitionNum,FilePath) -> FilePath -> IO ()
copyFileFromDisk extraEnv diskT ro phys_path (part,src_path) dst_path
  = withMountedDisk extraEnv diskT ro phys_path part $ \contents ->
      void $ readProcessOrDie "cp" [contents </> deslash src_path, dst_path] ""
