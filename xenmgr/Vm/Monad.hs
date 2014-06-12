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

{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Vm.Monad
    ( Vm
    , VmContext(..)
    , ContextMap
    , runVm
    , newVmContextMap
    , newVmContext
    , findVmContext
    , findVmContext_
    , vmUuid
    , vmMonitor
    , vmContext
    , vmSubmit
    , vmEvalEvent

    , withVmDbLock
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans

import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef

import Vm.Types
import Vm.Monitor
import XenMgr.Rpc
import XenMgr.Errors

import Tools.FreezeIOM
import Tools.Lock

-- | vm context
data VmContext
   = VmContext {
       vm_uuid :: Uuid
     , vm_monitor :: VmMonitor
     , vm_db_lock :: MVar ()
    }

-- | vm monad
newtype Vm a
      = Vm { unVm :: ReaderT VmContext Rpc a }
        deriving (Functor, Monad, MonadError XmError)

type ContextMap = MVar (Map Uuid VmContext)

runVm :: LiftRpc m => VmContext -> Vm a -> m a
runVm c f = liftRpc $ runReaderT (unVm f) c

instance MonadIO Vm where liftIO = Vm . lift . liftIO
instance LiftRpc Vm where liftRpc = Vm . lift

instance Applicative Vm where pure  = return
                              (<*>) = ap

instance FreezeIOM (RpcContext,VmContext) (Either XmError) Vm where
    freeze f = do
      vmc  <- vmContext
      rpcc <- liftRpc rpcGetContext
      liftIO $ f (rpcc, vmc)

    thaw = thaw_ where
        thaw_ :: forall a. (RpcContext,VmContext) -> Vm a -> IO (Either XmError a)
        thaw_ (rpcc,vmc) f = rpc rpcc $ (runVm vmc f :: Rpc a)

    cont (Left ex) = throwError ex
    cont (Right x) = return x

instance (MonadRpc XmError) Vm where
  rpcGetContext = liftRpc rpcGetContext
  rpcLocalContext ctxf f = vmContext >>= \c -> liftRpc (rpcLocalContext ctxf (runVm c f))

newVmContext :: Uuid -> VmMonitor -> Rpc VmContext
newVmContext uuid monitor
  = VmContext uuid monitor <$> liftIO (newMVar ())

newVmContextMap :: Rpc ContextMap
newVmContextMap = liftIO $ newMVar Map.empty

findVmContext :: (MonadIO m) => ContextMap -> Uuid -> m (Maybe VmContext)
findVmContext c uuid = liftIO . withMVar c $ return . Map.lookup uuid

findVmContext_ :: (MonadIO m) => ContextMap -> Uuid -> m VmContext
findVmContext_ c uuid = from =<< findVmContext c uuid where
    from Nothing  = liftIO (error $ "VM context for " ++ show uuid ++ " not found")
    from (Just x) = return x

vmUuid :: Vm Uuid
vmUuid = Vm $ vm_uuid <$> ask

vmMonitor :: Vm VmMonitor
vmMonitor = Vm $ vm_monitor <$> ask

vmContext :: Vm VmContext
vmContext = Vm ask

vmSubmit :: VmEvent -> Vm ()
vmSubmit e =
    do m <- vmMonitor
       liftRpc (submitVmEvent m e)

vmEvalEvent :: VmEvent -> Vm ()
vmEvalEvent e =
    do m <- vmMonitor
       liftRpc (evalVmEvent m e)

withVmDbLock :: Vm a -> Vm a
withVmDbLock f
  = vmContext >>= return . vm_db_lock >>= \lock -> withMvLock lock f

