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

module XenMgr.GuestReq
       (
         guestRequestAttention
       ) where

import Control.Applicative
import Control.Monad
import Data.String
import Data.Int
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text.Lazy as TL
import Control.Concurrent
import XenMgr.Rpc
import Rpc.Autogen.DbusClient
import Rpc.Autogen.XenmgrNotify
import XenMgr.Expose.ObjectPaths
import Vm.Types
import Vm.DomainCore
import Tools.Misc

maybeWithSender :: MonadRpc e m => (BusName -> m a) -> m (Maybe a)
maybeWithSender f = rpcGetSender >>= go where
  go Nothing  = return Nothing
  go (Just s) = Just <$> f s

maybeWithSenderUuid :: MonadRpc e m => (Uuid -> m a) -> m (Maybe a)
maybeWithSenderUuid f =
  fmap join $ maybeWithSender $ \sender ->
    let s = TL.unpack (strBusName sender) in do
      domid <- orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" s
      uuid  <- getDomainUuid domid
      case uuid of
        Nothing -> return Nothing
        Just u  -> Just <$> f u

guestRequestAttention :: Rpc ()
guestRequestAttention =
  void $ maybeWithSenderUuid $ \uuid ->
    notifyComCitrixXenclientXenmgrGuestreqRequestedAttention xenmgrObjectPath (show uuid) (vmObjPath uuid)

  
