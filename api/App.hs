{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module App ( App, AppState(..), runApp, initAppState, getAppState, liftRpc, module Rpc ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Control.Exception as E
import Data.IORef
import Error
import Rpc

-- Placeholder
type AppState = ()

newtype App a = App { unApp :: ReaderT AppState Rpc a }
    deriving (Functor, Monad, MonadIO, MonadError ApiError, MonadReader AppState)

initAppState :: IO AppState
initAppState = return ()

runApp :: AppState -> App a -> Rpc a
runApp state app = runReaderT (unApp app) state

getAppState :: App AppState
getAppState = ask

liftRpc = App . lift
