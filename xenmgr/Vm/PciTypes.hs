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

module Vm.PciTypes where

import Text.Printf
import qualified Data.Text as T

type VendorID     = Int
type DeviceID     = Int
type ClassID      = Int

-- PCI address
data PciAddr
   = PciAddr { pciDomain :: !Int
             , pciBus    :: !Int
             , pciSlot   :: !Int
             , pciFunc   :: !Int
             }
   deriving (Eq, Ord)

-- Todo: Just leave PciAddr as the key, and put the rest into PciInfo.
-- PCI device
data PciDev
   = PciDev { devAddr :: !PciAddr
            , devNameT :: !T.Text
            , devSysfsPathT :: !T.Text }
   deriving (Eq, Ord)

devName = T.unpack . devNameT
devSysfsPath = T.unpack . devSysfsPathT

-- PCI passthrough rule definition in database
-- defines how we find pci addresses of interesting devices for passthrough
data PciPtRule
   = PciPtRule
       { ruleClass     :: !(Maybe ClassID)
       , ruleVendor    :: !(Maybe VendorID)
       , ruleDevice    :: !(Maybe DeviceID)
         -- Do these devices require pci slot in guest being forced to same as real one
       , ruleForceSlot :: !Bool }
   | PciPtRuleBDF
       { ruleAddr :: PciAddr
       , ruleUseGuestSlot :: Maybe Int }
   deriving ( Eq )

data PciPtDev
   = PciPtDev { pciPtDevice :: !PciDev
              , pciPtGuestSlot :: !PciPtGuestSlot
              , pciPtMsiTranslate :: !Bool
              , pciPtSource :: !PciPtSource
              }
   deriving (Eq, Show, Ord)

data PciPtSource
   = SourceConfig | SourceVendorPlugin deriving (Eq,Show,Ord)

data PciPtGuestSlot
   = PciSlotDontCare | PciSlotMatchHost | PciSlotUse Int deriving (Eq,Show,Ord)

instance Show PciAddr where
    show x = printf "%04x:%02x:%02x.%x" (pciDomain x) (pciBus x) (pciSlot x) (pciFunc x)

instance Show PciDev where
    show dev = show (devAddr dev) ++ " " ++ devName dev

instance Show PciPtRule where
    show (PciPtRule cls vendor dev _) =
        printf "match %s %s %s" (maybe "" cls_descr cls) (maybe "" vendor_descr vendor) (maybe "" dev_descr dev)
        where
          cls_descr x    = printf "class=0x%04X" x
          vendor_descr x = printf "vendor=0x%04X" x
          dev_descr x    = printf "device=0x%04X" x
    show (PciPtRuleBDF addr Nothing)   = "bdf=" ++ show addr
    show (PciPtRuleBDF addr (Just sl)) = "bdf=" ++ show addr ++ " @ " ++ show sl
