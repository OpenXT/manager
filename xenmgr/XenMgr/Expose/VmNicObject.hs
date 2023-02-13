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

{-# LANGUAGE FlexibleContexts #-}

module XenMgr.Expose.VmNicObject (expose, unexpose) where

import Data.Maybe
import Control.Monad
import Control.Applicative
import Rpc.Autogen.VmNicServer
import XenMgr.Rpc
import XenMgr.Errors
import Vm.Types
import Vm.Queries
import Vm.Actions
import Tools.Log
import Data.Int
import Data.Maybe
import Data.String
import XenMgr.Expose.ObjectPaths
import XenMgr.XM
import Vm.Monad

data ID = ID { vm_uuid :: Uuid
             , nic_id  :: NicID }

expose :: Uuid -> NicID -> XM ()
expose vm_uuid nic_id = do
    let path = nicObjectPath vm_uuid nic_id
    info $ "exposing nic object " ++ show path
    xm <- xmContext
    let imp = implementation xm (ID vm_uuid nic_id)
    liftRpc $ rpcExpose path (interfaces imp)

unexpose :: Uuid -> NicID -> Rpc ()
unexpose vm_uuid nic_id = do
    let path = nicObjectPath vm_uuid nic_id
    info $ "removing nic object " ++ show path
    rpcHide path

implementation :: XmContext -> ID -> VmNicServer Rpc
implementation xm id@(ID vm nic_id) = self where
  runxm = runXM xm
  runvm = runXM xm . xmRunVm vm
  self = VmNicServer {
    comCitrixXenclientVmnicDelete = runvm $ _Delete id
  , comCitrixXenclientVmnicGetBackendUuid = _GetBackendUuid id
  , comCitrixXenclientVmnicSetBackendUuid = runvm . _SetBackendUuid id
  , comCitrixXenclientVmnicGetBackendName = _GetBackendName id
  , comCitrixXenclientVmnicSetBackendName = runvm . _SetBackendName id
  , comCitrixXenclientVmnicGetNetwork = _GetNetwork id
  , comCitrixXenclientVmnicSetNetwork = runxm . _SetNetwork id
  , comCitrixXenclientVmnicGetWirelessDriver = _GetWirelessDriver id
  , comCitrixXenclientVmnicSetWirelessDriver = runvm . _SetWirelessDriver id
  , comCitrixXenclientVmnicGetMac = _GetMac id
  , comCitrixXenclientVmnicGetMacActual = getVmNicMacActual vm nic_id
  , comCitrixXenclientVmnicSetMac = runvm . _SetMac id
  , comCitrixXenclientVmnicGetEnabled = _GetEnabled id
  , comCitrixXenclientVmnicSetEnabled = runvm . _SetEnabled id
  , comCitrixXenclientVmnicGetModel = _GetModel id
  , comCitrixXenclientVmnicSetModel = runvm . _SetModel id
  }

_Delete :: ID -> Vm ()
_Delete (ID uuid id) =
    do removeNicFromVm uuid id
       liftRpc $ unexpose uuid id

_show_uuid :: Uuid -> String
_show_uuid uuid = show uuid

_GetBackendUuid :: ID -> Rpc String
_GetBackendUuid id =  fromMaybe "" . fmap _show_uuid  <$> _nic_field id nicdefBackendUuid

_SetBackendUuid :: ID -> String -> Vm ()
_SetBackendUuid id uuid = _modify_nic id $ \n -> n { nicdefBackendUuid = case uuid of
                                                                           "" -> Nothing
                                                                           _  -> Just (fromString uuid) }

_show_backend :: String -> String
_show_backend backend = show backend

_GetBackendName :: ID -> Rpc String
_GetBackendName id =  fromMaybe "" . fmap _show_backend  <$> _nic_field id nicdefBackendName

_SetBackendName :: ID -> String -> Vm ()
_SetBackendName id name = _modify_nic id $ \n -> n { nicdefBackendName = case name of
                                                                           "" -> Nothing
                                                                           _  -> Just name }

_GetNetwork :: ID -> Rpc String
_GetNetwork id = networkToStr <$> _nic_field id nicdefNetwork

_SetNetwork :: ID -> String -> XM ()
_SetNetwork (ID vm nicid) network = changeVmNicNetwork vm nicid (networkFromStr network)

_GetWirelessDriver :: ID -> Rpc Bool
_GetWirelessDriver id = _nic_field id nicdefWirelessDriver

_SetWirelessDriver :: ID -> Bool -> Vm ()
_SetWirelessDriver id wifi = _modify_nic id $ \n -> n { nicdefWirelessDriver = wifi }

_GetMac :: ID -> Rpc String
_GetMac id = _nic_field id nicdefMac >>= return . toStr
    where
      toStr Nothing  = "auto"
      toStr (Just m) = m

_SetMac :: ID -> String -> Vm ()
_SetMac id "auto" = _modify_nic id $ \n -> n { nicdefMac = Nothing }
_SetMac id m      = _modify_nic id $ \n -> n { nicdefMac = Just m }

_GetEnabled :: ID -> Rpc Bool
_GetEnabled id = _nic_field id nicdefEnable

_SetEnabled :: ID -> Bool -> Vm ()
_SetEnabled id e = _modify_nic id $ \n -> n { nicdefEnable = e }

_GetModel :: ID -> Rpc String
_GetModel id = _nic_field id nicdefModel >>= return . toStr
    where
      toStr Nothing = "e1000"
      toStr (Just model) = model

_SetModel :: ID -> String -> Vm ()
_SetModel id ""    = _modify_nic id $ \n -> n { nicdefModel = Nothing }
_SetModel id model = _modify_nic id $ \n -> n { nicdefModel = Just model }

_nic_field  (ID uuid id) f =
    do nic <- getNic uuid id
       case nic of
         Nothing  -> failNoSuchNic
         Just nic -> return $ f nic

_modify_nic (ID uuid id) f = modifyVmNic uuid id f

