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

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, MultiParamTypeClasses, TypeFamilies, RankNTypes, UndecidableInstances#-}

module UpdateMgr.App ( App, AppState(..), runApp, initAppState, getAppState, liftRpc, module UpdateMgr.Rpc ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Control.Exception as E
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.IORef
import UpdateMgr.Types
import UpdateMgr.Rpc
import UpdateMgr.Error
import UpdateMgr.Curl (DownloadHandle)
import Tools.FreezeIOM
import Control.Concurrent.Lifted

data AppState = AppState { appCurrentDownload :: IORef (Maybe DownloadHandle)
                         , appCurrentDownloadSpeed :: IORef Float }

newtype App a = App { unApp :: ReaderT AppState Rpc a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError UpdateMgrError, MonadReader AppState, MonadBase IO)

-- TODO: See if new versions of ghc can derive this automatically.
instance MonadBaseControl IO App where
    type StM App a =  StM (ReaderT AppState Rpc) a
    liftBaseWith op = App $ liftBaseWith $ (\runInBase -> op (runInBase . unApp))
    restoreM = App . restoreM

initAppState :: IO AppState
initAppState = do
  dl <- newIORef Nothing
  speed <- newIORef 0
  return AppState { appCurrentDownload = dl
                  , appCurrentDownloadSpeed = speed }

runApp :: AppState -> App a -> Rpc a
runApp state app = runReaderT (unApp app) state

getAppState :: App AppState
getAppState = ask

liftRpc = App . lift
