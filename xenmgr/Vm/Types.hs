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

module Vm.Types (
                 AcpiState
               , VmType (..)
               , VmState (..)
               , CpuidResponse (..)
               , PorticaStatus (..)
               , VmGraphics(..)
               , VgpuMode(..)
               , VmConfig(..)
               , SupportedOS(..)
               , XcVersion(..)
               , S3Mode(..)
               , S4Mode(..)
               , osFromStr, osToStr

               , module Vm.Uuid
               , module Vm.DmTypes
               , module Vm.PciTypes
               ) where

import Data.Int
import Data.Char
import Data.List
import Data.String
import qualified Data.Map as M
import Text.Printf

import Tools.Misc
import Vm.Uuid
import Vm.DmTypes
import Vm.PciTypes

type AcpiState = Int

data VmType = Svm
            | ServiceVm Tag
              deriving (Eq, Show)
type Tag = String

data VmState = Running
             | PreCreate
             | CreatingDomain
             | CreatingDevices
             | Created
             | Shutdown
             | ShuttingDown
             | Rebooting
             | Rebooted
             | Suspending
             | Suspended
             | Restoring
             | Paused
                deriving (Eq, Show)

-- TODO: this could use better typing
newtype CpuidResponse = CpuidResponse String

-- Status of Portica Installation on client
data PorticaStatus = PorticaStatus {
      porticaInstalled :: Bool
    , porticaEnabled :: Bool
    } deriving (Eq, Show)

data VgpuMode = VgpuMode {
      vgpuMaxVGpus :: Int
    , vgpuName :: String
    , vgpuMsiTranslate :: Bool
    , vgpuPciPtDevices :: [PciPtDev]
    } deriving (Eq, Show)

data VmGraphics = HDX | VGAEmu deriving (Eq,Show)

data XcVersion = XcVersion String deriving (Eq,Ord)

data S3Mode = S3Pv | S3Ignore | S3Restart | S3Snapshot deriving (Eq,Show)
data S4Mode = S4Pv | S4Ignore | S4Restart | S4Snapshot deriving (Eq,Show)

-- Vm config type.
data VmConfig = VmConfig {
      vmcfgUuid :: Uuid
    , vmcfgName :: Maybe String
    , vmcfgQemuDmPath :: FilePath
    , vmcfgQemuDmTimeout :: Int
    , vmcfgKernelPath :: Maybe String
    , vmcfgCpuidResponses :: [CpuidResponse]
    , vmcfgXciCpuidSignature :: Bool
    , vmcfgOs :: SupportedOS
    , vmcfgNics :: [NicDef]
    , vmcfgDisks :: [Disk]
    , vmcfgNetworks :: [NetworkInfo]
    , vmcfgCryptoKeyDirs :: FilePath
    , vmcfgPvAddons :: Bool
    , vmcfgGraphics :: VmGraphics
    , vmcfgRestrictDisplayDepth :: Bool
    , vmcfgRestrictDisplayRes :: Bool
    , vmcfgOemAcpiFeatures :: Bool
    , vmcfgVgpuMode :: Maybe VgpuMode
    , vmcfgPciPtDevices :: [PciPtDev]
    , vmcfgCdExclusive :: Bool
    , vmcfgAutostart :: Bool
    , vmcfgSeamlessSupport :: Bool
    , vmcfgSmbios :: String -- path to extra smbios table
    , vmcfgAcpi :: String -- path to extra acpi table
    , vmcfgXcVersion :: XcVersion
    , vmcfgUsbEnabled :: Bool
    , vmcfgUsbAutoPassthrough :: Bool
    , vmcfgStubdom :: Bool
    , vmcfgStubdomMemory :: Int
    , vmcfgStubdomCmdline :: Maybe String
    , vmcfgMemoryMib :: Int
    , vmcfgMemoryMinMib :: Int
    , vmcfgMemoryStaticMaxMib :: Int
    , vmcfgPreserveOnReboot :: Bool
    }

data SupportedOS = Linux | Windows | Windows8 | UnknownOS deriving (Eq,Ord)

osMap =
    M.fromList
         [ (Linux, "linux")
         , (Windows, "windows")
         , (Windows8, "windows8")
         , (UnknownOS, "unknown") ]

osMapRev = M.fromList . map flip_ . M.toList $ osMap where flip_ (a,b) = (b,a)

osToStr os    = str where Just str = os `M.lookup` osMap
osFromStr str = str `M.lookup` osMapRev

