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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Posix
import System.Posix.Syslog
import System.IO
import System.Exit
import System.Environment
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.IORef
import Data.String

import Tools.Log
import Tools.IfM
import Tools.Misc
import Rpc.Core

import qualified RpcProxyObject as RPO
import RpcProxy
import RpcProxyM
import RulesParser
import Rules
import Settings
import Types
import RulesCache

main = do
  settings <- getSettings <$> getArgs
  withSyslog "rpc-proxy" [] USER . E.handle (\(ex::E.SomeException) -> warn (show ex)) . runClient_ $ do
        rulesCache <- mkRulesCache settings
        -- re-read rules on SIGHUP
        liftIO $ installHandler sigHUP (Catch $ () <$ dropCache rulesCache) Nothing

        if runDbusServer settings then
          -- with dbus server responding to UID requests
          -- bracket rpc server connection to make sure it is stopped on fatal exceptions
          liftIO . rpcServe "com.citrix.xenclient.rpc_proxy" $ \rpcContext -> do
              status <- runRpcProxyM rpcContext $ RPO.expose rulesCache
              case status of
                  Left err -> warn (show err)
                  _        -> return ()
              -- ToDo: proxy is mentioned twice, because here we want
              --       it vulnerable to problems in the RPO.expose
              --       above.  (I.e. if expose dies due to exception
              --       raised, then proxy should die to.)
              liftIO $ proxy rulesCache settings
         else liftIO $ proxy rulesCache settings

runClient :: RpcProxy a -> IO (Either RpError a)
runClient client = flip runRpcProxyM client =<< rpcConnect


-- ToDo: Somehow void doesn't seem to get picked up from Tools.Misc on my system.
--       remove the local definition, after testing it's alright on different system.
runClient_ = void . runClient
 where void = (() <$)
