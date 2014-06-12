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
import System.Environment (getArgs)
import System.IO
import System.Posix.Syslog
import System.Posix.Process
import System.Posix.Signals
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad

import Tools.Log
import qualified StoreHouse.StoreHouseRpc
import qualified StoreHouse.VdiObject
import qualified StoreHouse.SrObject

import StoreHouse.Rpc
import StoreHouse.Vdi
import StoreHouse.SR
import StoreHouse.Ctx
import Tools.Uuid (uuidNull)

service = "com.citrix.xenclient.storehouse"

exposeObjects ctx = do
	sruuids <- liftIO (ctxGetSrUuids ctx)
	mapM_ exposeSRAndVdis sruuids
	where
		exposeSRAndVdis sruuid = do
			sr       <- liftIO $ ctxGetSr ctx sruuid
			vdiuuids <- liftIO $ srListVdis sr
			StoreHouse.SrObject.expose ctx sruuid
			mapM_ (addAndExposeVDI sruuid) vdiuuids
		addAndExposeVDI sruuid uuid = do
			liftIO $ ctxAddVdi ctx (uuid, sruuid)
			StoreHouse.VdiObject.expose ctx uuid

initialContext :: Bool -> IO Ctx
initialContext user = do
	ctx <- ctxNew
	initialSR <- physSRNew (if user then "/tmp/storehouse/" else "/storage/vdis/")
	-- add SR to context and expose interface
	ctxAddSr ctx $ (uuidNull, initialSR)
	return ctx

main :: IO ()
main = do
	args <- getArgs
	let user = case args of
		("--user" : xs) -> True
		_               -> False
	ctx <- initialContext user
	withSyslog "storehouse" [] USER . (rpcServeOn user) service $ \rpcContext -> do
		r <- E.try $ do
			debug "starting.."
			status <- rpc rpcContext $ do
				exposeObjects ctx
				StoreHouse.StoreHouseRpc.expose
				-- live forever
				liftIO . forever $ threadDelay (10^6 * 60)
			case status of
				Left error -> fatal $ "error during initialisation " ++ show error
				Right _    -> return ()
		case r of
			Right () -> return ()
			Left (ex :: E.SomeException) -> fatal $ "Caught error: " ++ show ex
