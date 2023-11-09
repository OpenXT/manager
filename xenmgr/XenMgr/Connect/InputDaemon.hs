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

-- Simple interface to input daemon
module XenMgr.Connect.InputDaemon (
                         inputAuthOnBoot
                       , inputRunAuthOnBoot
                       , inputAuthSetContext
                       , inputAuthSetContextFlags
                       , inputOnSignal
                       , inputGetFocusedDomainID
                       , inputSwitchFocus
                       , inputLock
                       , inputUpdateSeamlessMouseSettings
                       , inputSetSlot
                       , auth_FLAG_REMOTE_USER
                       , auth_FLAG_USER_HASH
                   ) where

import Data.String
import Data.Bits
import Data.Int
import Rpc.Autogen.InputDaemonClient
import XenMgr.Rpc
import Vm.Types

call f = f "com.citrix.xenclient.input" "/"

auth_FLAG_REMOTE_USER, auth_FLAG_USER_HASH :: Int32
auth_FLAG_REMOTE_USER = 1 `shiftL` 19
auth_FLAG_USER_HASH = 1 `shiftL` 23

inputAuthSetContext :: String -> String -> Rpc ()
inputAuthSetContext user title = call comCitrixXenclientInputAuthSetContext user title

inputAuthSetContextFlags :: String -> String -> Int32 -> Rpc ()
inputAuthSetContextFlags user title flags = call comCitrixXenclientInputAuthSetContextFlags user title flags

inputAuthOnBoot :: Rpc Bool
inputAuthOnBoot = call comCitrixXenclientInputGetAuthOnBoot

inputRunAuthOnBoot :: Rpc Bool
inputRunAuthOnBoot = call comCitrixXenclientInputAuthBegin

inputUpdateSeamlessMouseSettings :: Uuid -> Rpc ()
inputUpdateSeamlessMouseSettings uuid = call comCitrixXenclientInputUpdateSeamlessMouseSettings (show uuid)

inputLock :: Rpc ()
inputLock = call comCitrixXenclientInputLock True

inputGetFocusedDomainID :: Rpc (Maybe DomainID)
inputGetFocusedDomainID =
    do domid <- call comCitrixXenclientInputGetFocusDomid
       if domid <= 0
          then return Nothing
          else return $ Just domid

inputSwitchFocus :: MonadRpc e m => DomainID -> m Bool
inputSwitchFocus domid = call comCitrixXenclientInputSwitchFocus domid False

inputSetSlot :: DomainID -> Int -> Rpc ()
inputSetSlot domid slot = call comCitrixXenclientInputSetSlot domid (fromIntegral slot)

inputOnSignal :: String -> (RpcSignal -> Rpc ()) -> Rpc ()
inputOnSignal = rpcOnSignalFrom inputDaemon

inputDaemon :: Proxy
inputDaemon =
    Proxy inputDaemonObj $ fromString "com.citrix.xenclient.input"
  where
    inputDaemonObj = RemoteObject (fromString "com.citrix.xenclient.input") (fromString "/")
