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

module Vm.Dm
    ( DmFront(..), DmDevType(..), DmDevState(..)
    , getFrontDevices
    , moveBackend
    ) where

import Control.Monad
import Control.Applicative
import Data.String
import Data.Maybe
import Text.Printf

import Vm.DomainCore
import Vm.DmTypes
import Vm.Uuid

import qualified XenMgr.Connect.Xl as Xl
import XenMgr.Rpc

import Tools.XenStore
import Tools.Misc
import Tools.Log
import Tools.Text

devicesXSPath :: DomainID -> XSPath
devicesXSPath domid = printf "%s/device" (domainXSPath domid)

data DmDevType = VBD | VIF | VWIF | VKBD | V4V
                 deriving ( Eq, Show )

data DmFront = DmFront { dmfType :: DmDevType
                       , dmfID :: XbDeviceID
                       , dmfDomid :: DomainID
                       , dmfState :: DmDevState
                       , dmfBackPath :: Maybe XSPath
                       , dmfBackUuid :: Uuid
                       , dmfBackDomid :: DomainID
                       }
               deriving ( Eq, Show )

data DmDevState = Unknown | Initialising | InitWait | Initialised | Connected | Closing | Closed
                  deriving (Eq,Show)

dmDevStateFromInt :: Int -> DmDevState
dmDevStateFromInt 1 = Initialising
dmDevStateFromInt 2 = InitWait
dmDevStateFromInt 3 = Initialised
dmDevStateFromInt 4 = Connected
dmDevStateFromInt 5 = Closing
dmDevStateFromInt 6 = Closed
dmDevStateFromInt _ = Unknown

dmDevStateToInt :: DmDevState -> Int
dmDevStateToInt Unknown = 0
dmDevStateToInt Initialising = 1
dmDevStateToInt InitWait = 2
dmDevStateToInt Initialised = 3
dmDevStateToInt Connected = 4
dmDevStateToInt Closing = 5
dmDevStateToInt Closed = 6

frontXSPath :: DmDevType -> DomainID -> XSPath
frontXSPath t domid =
    devicesXSPath domid ++ "/" ++ d where
        d = case t of
              VBD  -> "vbd"
              VIF  -> "vif"
              VWIF -> "vwif"
              VKBD -> "vkbd"
              V4V  -> "v4v"

backXSPath :: DmDevType -> DomainID -> DomainID -> XSPath
backXSPath t frontdomid backdomid =
    devicesXSPath backdomid ++ "/backend/" ++ d ++ "/" ++ show frontdomid where
        d = case t of
              VBD  -> "vbd"
              VIF  -> "vif"
              VWIF -> "vif"
              VKBD -> "vkbd"
              V4V  -> "v4v"

getFrontDevices :: DmDevType -> DomainID -> IO [DmFront]
getFrontDevices t domid =
    catMaybes <$> (mapM fromid =<< xsDir dir)
    where
      dir = frontXSPath t domid
      fromid idstr = case maybeRead idstr of
                       Just id -> getFrontDevice t domid (XbDeviceID id)
                       Nothing -> return Nothing

getFrontDevice :: DmDevType -> DomainID -> XbDeviceID -> IO (Maybe DmFront)
getFrontDevice t domid id@(XbDeviceID devid) = do
    let path = dir ++ "/" ++ (show devid)
        dir  = frontXSPath t domid
    statestr_ <- xsRead $ path ++ "/state"
    backpath_ <- xsRead $ path ++ "/backend"
    backdomid_ <- xsRead $ path ++ "/backend-id"
    backdomuuid_ <- fmap fromString <$> xsRead (path ++ "/backend-uuid")
    return $ do
      statestr <- statestr_
      state <- dmDevStateFromInt <$> maybeRead statestr
      backdomid <- maybeRead =<< backdomid_
      backdomuuid <- if backdomid == 0 then return domain0uuid else backdomuuid_
      return DmFront {
                   dmfType = t
                 , dmfID = id
                 , dmfDomid = domid
                 , dmfState = state
                 , dmfBackPath = backpath_
                 , dmfBackDomid = backdomid
                 , dmfBackUuid = backdomuuid
                 }

moveBackend :: DmDevType -> DomainID -> XbDeviceID -> DomainID -> Rpc ()
moveBackend t frontdomid id backdomid = do
    dev <- liftIO $ getFrontDevice t frontdomid id
    case dev of
      Nothing  -> warn "moveBackend: failed to read device information"
      Just dev | dmfBackDomid dev == backdomid -> return () -- backend already at destination
               | otherwise ->
                   do case t of
                        VIF  -> moveNIC dev
                        VWIF -> moveNIC dev
                        _    -> error $ "cannot move backend for device type " ++ show t
    where
      moveNIC dev =
          do info $ printf "moving NIC (%s) backend to domid=%d" (show dev) backdomid
             uuid <- fromMaybe (error "failed to get domain UUID") <$> getDomainUuid frontdomid
             liftIO $ Xl.setNicBackendDom uuid id backdomid
