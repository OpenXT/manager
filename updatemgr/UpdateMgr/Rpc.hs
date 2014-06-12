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

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, MultiParamTypeClasses, TypeFamilies #-}

module UpdateMgr.Rpc ( Rpc, rpc, module Rpc.Core ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Control.Exception as E
import Rpc.Core
import Tools.FreezeIOM

import UpdateMgr.Error

newtype Rpc a = Rpc { unRpc :: RpcM UpdateMgrError a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError UpdateMgrError, MonadRpc UpdateMgrError, FreezeIOM RpcContext (Either UpdateMgrError), MonadBase IO)

instance MonadBaseControl IO Rpc where
    newtype StM Rpc a = StRpc { unStRpc :: StM (RpcM UpdateMgrError) a }
    liftBaseWith op = Rpc . liftBaseWith $ op . \runInBase ->
        liftM StRpc . runInBase . unRpc
    restoreM = Rpc . restoreM . unStRpc

-- ToDo: Move these istances (and their copies in rpc-proxy) to
-- xch-rpc, where they logically belong.
instance (IsRemoteError e) => MonadBase IO (RpcM e) where
    liftBase = liftIO

instance (IsRemoteError e) => MonadBaseControl IO (RpcM e) where
    newtype StM (RpcM e) a = StRpcM {unStRpcM :: (Either e) a}
    liftBaseWith op = freeze $ op . \ctx -> liftM StRpcM . thaw ctx
    restoreM = cont . unStRpcM

rpc ctx (Rpc f) = runRpcM f ctx
