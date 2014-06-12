{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module Rpc ( Rpc, rpc, module Rpc.Core ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import qualified Control.Exception as E
import Rpc.Core
import Error
import Tools.FreezeIOM

newtype Rpc a = Rpc (RpcM ApiError a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError ApiError, MonadRpc ApiError, FreezeIOM RpcContext (Either ApiError))

rpc ctx (Rpc f) = runRpcM f ctx