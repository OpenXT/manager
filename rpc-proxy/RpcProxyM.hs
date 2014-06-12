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

{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, ExistentialQuantification, TypeSynonymInstances, RankNTypes, ScopedTypeVariables, StandaloneDeriving, ViewPatterns, NoMonomorphismRestriction #-}
module RpcProxyM where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error
import Rpc.Core
import Tools.FreezeIOM
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader

data RpError = RpError String RemoteErr

instance IsRemoteError RpError where
    fromRemoteErr call err = RpError ("failed remote call: " ++ show call ++ ": " ++ show err) err
    toRemoteErr (RpError _ dbusE) = Just dbusE

instance Show RpError where
    show (RpError msg _) = msg

newtype RpcProxy a
      = RpcProxy { unRpcProxy :: RpcM RpError a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadError RpError, MonadRpc RpError, MonadReader RpcContext, FreezeIOM RpcContext (Either RpError), MonadBase IO)

-- Unfortunately, deriving (MonadBaseControl IO) doesn't work.

runRpcProxyM :: RpcContext -> RpcProxy a -> IO (Either RpError a)
runRpcProxyM c f = runRpcM (unRpcProxy f) c

-- re-using the instance for RpcM

instance MonadBaseControl IO RpcProxy where
    newtype StM RpcProxy a = StRpcProxy {unSt :: Either RpError a}
    liftBaseWith op = RpcProxy . liftBaseWith $
                      op . \rib -> liftM (StRpcProxy . unStRpcM) . rib . unRpcProxy
    restoreM = RpcProxy . restoreM . StRpcM . unSt

-- -- Or directly implementing the instance:

-- instance MonadBaseControl IO RpcProxy where
--     newtype StM RpcProxy a = StRpcProxy {unSt :: Either RpError a}
--     liftBaseWith op = freeze (op . \ctx -> liftM StRpcProxy . thaw ctx . unRpcProxy)
--     restoreM = cont . unSt

instance (IsRemoteError e) => MonadBase IO (RpcM e) where
    liftBase = liftIO

instance (IsRemoteError e) => MonadBaseControl IO (RpcM e) where
    newtype StM (RpcM e) a = StRpcM {unStRpcM :: (Either e) a}
    liftBaseWith op = freeze $ op . \ctx -> liftM StRpcM . thaw ctx
    restoreM = cont . unStRpcM
