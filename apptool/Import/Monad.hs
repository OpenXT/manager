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

{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Import.Monad
       (
         ImportContext (..)
       , ImportState (..)
       , Import
       , importContext
       , options
       , app
       , addedFiles
       , addImportFile
       , runImportRpc
       , runImport
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Error

import qualified Control.Exception as E
import System.FilePath
import Rpc.Core
import Tools.FreezeIOM

import Appliance
import Import.Types

data ImportContext
   = ImportContext {
         ovfRootPath :: FilePath
       , importOptions :: ImportOptions
       , importAppID :: AppID
       , importState :: MVar ImportState
       }

data ImportState
   = ImportState { importAddedFiles :: [FilePath] }

newtype ImportRpc a = ImportRpc (RpcM ImportError a)
    deriving (Functor, Applicative, Monad, MonadError ImportError, MonadRpc ImportError, FreezeIOM RpcContext (Either ImportError))

rpc ctx (ImportRpc f) = runRpcM f ctx
runImportRpc = rpc

instance MonadIO ImportRpc where
    liftIO act =  ImportRpc $
        do status <- liftIO $ E.try act
           -- translate io errors into monadic version
           tunnel status
        where
          tunnel :: Either E.SomeException a -> RpcM ImportError a
          tunnel (Right v)  = return v
          tunnel (Left err) = throwError (ImportIOError $ show err)

newtype Import a = Import { unImport :: ReaderT ImportContext ImportRpc a }
                   deriving ( Functor, Applicative, MonadIO, Monad
                            , MonadError ImportError
                            , MonadReader ImportContext  )

importContext = Import ask
options = Import (importOptions <$> ask)
app = importAppID <$> importContext

state :: Import (MVar ImportState)
state = importState <$> importContext

getState = liftIO . readMVar =<< state

addedFiles :: Import [FilePath]
addedFiles = importAddedFiles <$> getState

addImportFile :: FilePath -> Import ()
addImportFile f =  do
  s <- state
  liftIO $ modifyMVar_ s (\s -> return $ s { importAddedFiles = importAddedFiles s ++ [f] })

runImport :: ImportContext -> Import a -> ImportRpc a
runImport c i = runReaderT (unImport i) c
  where s0 = ImportState []

instance MonadRpc ImportError Import where
  rpcGetContext = Import $ lift rpcGetContext
  rpcLocalContext ctxf f = importContext >>= \ic -> ( Import $ lift (rpcLocalContext ctxf $ runImport ic f ) )

