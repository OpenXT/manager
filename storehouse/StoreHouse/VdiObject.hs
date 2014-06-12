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

module StoreHouse.VdiObject (expose, unexpose) where

import Control.Applicative ((<$>))

import StoreHouse.Rpc
import StoreHouse.Vdi
import Rpc.Autogen.StorageVdiServer
import StoreHouse.ObjectPaths
import StoreHouse.Ctx
import StoreHouse.SR

import Tools.Uuid
import Tools.Log
import qualified Tools.TapControl as Tap

expose :: Ctx -> Uuid KindVDI -> Rpc ()
expose ctx uuid = do
	rpcExpose (vdiObjectPath uuid) (interfaces $ implementationFor ctx uuid)
	info $ "exposed VDI " ++ show uuid ++ " over DBUS"

unexpose :: Uuid KindVDI -> Rpc ()
unexpose uuid = info ("removing VDI object " ++ show path) >> rpcHide path
	where path = vdiObjectPath uuid

implementationFor ctx uuid = StorageVdiServer
	{
	-- properties
	  comCitrixXenclientStorehouseVdiGetPhysType = liftIO (show <$> vdiGetPhysType ctx uuid)
	, comCitrixXenclientStorehouseVdiGetType     = return "disk"
	-- methods
	, comCitrixXenclientStorehouseVdiOpen      = liftIO $ cmdVdiOpen ctx uuid
	, comCitrixXenclientStorehouseVdiClose     = liftIO $ cmdVdiClose ctx uuid
	, comCitrixXenclientStorehouseVdiDelete    = \delNodes -> liftIO $ cmdVdiDelete ctx uuid delNodes
	, comCitrixXenclientStorehouseVdiFork      = cmdVdiFork
	, comCitrixXenclientStorehouseVdiListNodes = liftIO $ cmdVdiListNodes ctx uuid
	, comCitrixXenclientStorehouseVdiSnapshot  = liftIO $ cmdVdiSnapshot ctx uuid
	, comCitrixXenclientStorehouseVdiRevert    = cmdVdiRevert
	, comCitrixXenclientStorehouseVdiCheck     = cmdVdiCheck
	}

vdiGetPhysType ctx uuid =
	maybe (error "not a valid vdi object") vdiInfoType <$> vdiGetInfo ctx uuid

vdiGetPhysPath ctx uuid =
	maybe (error "not a valid vdi object") vdiInfoPhysicalPath <$> vdiGetInfo ctx uuid

vdiGetInfo ctx uuid = (flip srGetVdiInfo uuid) =<< ctxGetVdiSr ctx uuid

vdiGetPath ctx uuid = do
	sr <- ctxGetVdiSr ctx uuid
	maybe (error "no vdi path") id <$> srVdiPath sr uuid

cmdVdiOpen ctx uuid = do
	path <- vdiGetPath ctx uuid
	alreadyTapped <- ctxGetTap ctx path
	case alreadyTapped of
		Nothing -> do
			(devpath, devn) <- Tap.create path Tap.DiskTypeVhd Tap.DiskModeReadWrite Nothing
			ctxAddTap ctx (path, (devpath, devn))
			return devpath
		Just (devpath, _) -> return devpath

cmdVdiClose ctx uuid = do
	path <- vdiGetPath ctx uuid
	alreadyTapped <- ctxGetTap ctx path
	case alreadyTapped of
		Nothing         -> return ()
		Just (_, minor) -> do
			Tap.destroy Nothing minor
			ctxRemoveTap ctx path

cmdVdiDelete ctx uuid delNodes = do
	cmdVdiClose ctx uuid
	sr <- ctxGetVdiSr ctx uuid
	srDeleteVdi sr uuid delNodes

-- when you implement fork, make sure you also update srDeleteVdi so it
-- doesn't prune common branches
cmdVdiFork = undefined

cmdVdiListNodes ctx uuid = do
    sr <- ctxGetVdiSr ctx uuid
    srVdiListNodes sr uuid

cmdVdiSnapshot ctx uuid = do
	sr <- ctxGetVdiSr ctx uuid
	srSnapshotVdi sr uuid

cmdVdiRevert = undefined
cmdVdiCheck = undefined
