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

import Data.String
import Control.Applicative
import System.IO
import System.Posix.Syslog
import System.Posix.Process
import System.Posix.Signals
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad

import Tools.Log
import qualified UpdateMgr.UpdateMgrRpc

import UpdateMgr.Rpc
import UpdateMgr.UpdateMonad
import UpdateMgr.Logic
import UpdateMgr.Background
import UpdateMgr.App

import UpdateMgr.Poller

-- We need these services in order to start updatemgr at all
prerequisites :: [String]
prerequisites =
    [
     "com.citrix.xenclient.db"
    ]

waitPrerequisites :: Rpc ()
waitPrerequisites = mapM_ rpcWaitForService prerequisites

main :: IO ()
main =
    withSyslog "updatemgr" [] User . rpcServe "com.citrix.xenclient.updatemgr" $ \rpcContext ->
        do r <- E.try $
                do debug "starting.."
                   primary_job <- initJob
                   app_state <- initAppState
                   status <-
                       rpc rpcContext $ runApp app_state $
                       do -- wait until prerequisites are connected to dbus
                         info "waiting for prerequisite services to appear on DBUS.."
                         liftRpc waitPrerequisites
                         -- TODO: handing over the runner as an
                         -- argument might not be the best design.
                         monitorSymlink runner
                         UpdateMgr.UpdateMgrRpc.expose primary_job
                         -- continue existing update if any
                         backgroundUpdate primary_job continueUpdate
                         -- live forever
                         liftIO . forever $ threadDelay (10^6 * 60)
                   case status of
                     Left error -> fatal $ "error during initialisation " ++ show error
                     Right _    -> return ()
           case r of
             Right () -> return ()
             Left (ex :: E.SomeException) -> fatal $ "Caught error: " ++ show ex
