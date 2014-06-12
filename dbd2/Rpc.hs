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

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, PackageImports, FlexibleInstances, MultiParamTypeClasses #-}
module Rpc where

import Data.String

import Rpc.Autogen.DbServer (DbServer, interfaces)
import Rpc.Core
import Control.Applicative
import "mtl" Control.Monad.Error

import Tools.FreezeIOM

dbObjectPath :: ObjectPath
dbObjectPath = fromString "/"

service = "com.citrix.xenclient.db"

data RpcError = RpcError { remoteErr :: Maybe RemoteErr, localErr :: Maybe String }
    deriving (Show)

instance IsRemoteError RpcError where
    fromRemoteErr _    remote_err = RpcError (Just remote_err) Nothing
    toRemoteErr e                 = remoteErr e

newtype Rpc a = Rpc (RpcM RpcError a)
    deriving (Functor, Applicative, Monad, MonadIO
             , MonadError RpcError, MonadRpc RpcError, FreezeIOM RpcContext (Either RpcError))

expose :: DbServer Rpc -> Rpc ()
expose imp = rpcExpose dbObjectPath (interfaces $ imp)

rpc ctx (Rpc f) = runRpcM f ctx
