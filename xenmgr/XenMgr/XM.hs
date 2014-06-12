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

{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, MultiParamTypeClasses , FlexibleInstances #-}
module XenMgr.XM
    ( XM
    , XmContext(vmmap)
    , runXM
    , newXmContext
    , xmContext
    , xmDoesVmContextExist
    , xmCreateAndRegisterVmContext
    , xmWithVmCreationLock
    , xmWithVmSlotLock
    , xmWithBalloonLock
    , xmWithNetsyncLock
    , xmRunVm
    , module XenMgr.Rpc
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Concurrent
import Data.Maybe
import Tools.FreezeIOM
import Tools.Lock
import qualified Data.Map as Map

import Vm.Monad
import Vm.Monitor (VmMonitor)
import Vm.Types
import XenMgr.Rpc
import XenMgr.Errors

-- | xenmgr context
data XmContext
   = XmContext {
         vmmap :: ContextMap
       , lock_vm_creation :: MVar ()
       , lock_vm_slot :: MVar ()
       , lock_balloon :: MVar ()
       , lock_netsync :: MVar ()
     }

-- | xenmgr monad
newtype XM a
      = XM { unXM :: ReaderT XmContext Rpc a }
        deriving (Functor, Monad, MonadError XmError)

runXM :: LiftRpc m => XmContext -> XM a -> m a
runXM c f = liftRpc $ runReaderT (unXM f) c

instance MonadIO XM where liftIO = XM . lift . liftIO
instance LiftRpc XM where liftRpc = XM . lift
instance Applicative XM where pure = return
                              (<*>) = ap

instance FreezeIOM (RpcContext,XmContext) (Either XmError) XM where
    freeze f = do
      xmc  <- xmContext
      rpcc <- liftRpc rpcGetContext
      liftIO $ f (rpcc, xmc)

    thaw = thaw_
      where
        thaw_ :: forall a. (RpcContext,XmContext) -> XM a -> IO (Either XmError a)
        thaw_ (rpcc,xmc) f =
          rpc rpcc $ (runXM xmc f :: Rpc a)

    cont (Left ex) = throwError ex
    cont (Right x) = return x

instance (MonadRpc XmError) XM where
  rpcGetContext = liftRpc rpcGetContext
  rpcLocalContext ctxf f = xmContext >>= \c -> liftRpc (rpcLocalContext ctxf (runXM c f))

newXmContext :: Rpc XmContext
newXmContext = XmContext `fmap` newVmContextMap
                           `ap` liftIO (newMVar ())
                           `ap` liftIO (newMVar ())
                           `ap` liftIO (newMVar ())
                           `ap` liftIO (newMVar ())

xmContext :: XM XmContext
xmContext = XM ask

xmDoesVmContextExist :: Uuid -> XM Bool
xmDoesVmContextExist uuid =
  xmWithVmmap $ \vmm -> do
    kv <- liftIO (readMVar vmm)
    return $ isJust $ Map.lookup uuid kv

xmCreateAndRegisterVmContext :: Uuid -> VmMonitor -> XM VmContext
xmCreateAndRegisterVmContext uuid m =
    xmWithVmmap $ \vmm ->
        do c <- liftRpc $ newVmContext uuid m
           liftIO . modifyMVar vmm $ \kv ->
               case Map.lookup uuid kv of
                 Just _ -> error $ "VM context " ++ show uuid ++ " already created!"
                 _ -> return (Map.insert uuid c kv, c)

xmWithVmmap :: (ContextMap -> XM a) -> XM a
xmWithVmmap f = xmContext >>= f . vmmap

xmWithLock :: forall a. MVar () -> XM a -> XM a
xmWithLock lock f = withMvLock lock f
xmWithVmCreationLock f  = xmContext >>= \c -> xmWithLock (lock_vm_creation c) f
xmWithVmSlotLock f      = xmContext >>= \c -> xmWithLock (lock_vm_slot c) f
xmWithBalloonLock f     = xmContext >>= \c -> xmWithLock (lock_balloon c) f
xmWithNetsyncLock f     = xmContext >>= \c -> xmWithLock (lock_netsync c) f

xmRunVm :: Uuid -> Vm a -> XM a
xmRunVm uuid f = xmWithVmmap $ \vmap ->
  do c' <- findVmContext_ vmap uuid
     liftRpc (runVm c' f)

