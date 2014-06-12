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

module StoreHouse.SrObject (expose, unexpose) where

import Control.Applicative ((<$>))
import StoreHouse.Rpc
import StoreHouse.Vdi
import Rpc.Autogen.StorageSrServer
import StoreHouse.ObjectPaths
import StoreHouse.Ctx
import StoreHouse.SR
import qualified StoreHouse.VdiObject as VdiObject
import qualified Data.ByteString as B

import Tools.Uuid
import Tools.Log

expose :: Ctx -> Uuid KindSR -> Rpc ()
expose ctx uuid = do
	rpcExpose (srObjectPath uuid) (interfaces $ implementationFor ctx uuid)
	info $ "exposed SR " ++ show uuid ++ " over DBUS"

unexpose :: Uuid KindSR -> Rpc ()
unexpose uuid = info ("removing SR object " ++ show path) >> rpcHide path
	where path = srObjectPath uuid


implementationFor ctx uuid = StorageSrServer
    { comCitrixXenclientStorehouseSrGetName      = return "my-sr"
    , comCitrixXenclientStorehouseSrSetName      = \_ -> return ()
    , comCitrixXenclientStorehouseSrGetTotalSize = onSR srGetTotalSize
    , comCitrixXenclientStorehouseSrGetFreeSize  = onSR srGetFreeSize
    , comCitrixXenclientStorehouseSrCreateVdi    = \sz -> do
        vdiuuid <- onSR (\sr -> create sr sz)
        VdiObject.expose ctx vdiuuid
        return (vdiObjectPath vdiuuid)
    , comCitrixXenclientStorehouseSrListVdis     = onSR (\sr -> srListVdis sr >>= return . map vdiObjectPath)
    , comCitrixXenclientStorehouseSrListNodes      = onSR (\sr -> srListNodes sr)
    , comCitrixXenclientStorehouseSrCreateVdiWith  = \n -> onSR (\sr -> srCreateVdiWith sr n >>= return . vdiObjectPath)
    , comCitrixXenclientStorehouseSrStreamDownload = onSR srStreamDownload
    , comCitrixXenclientStorehouseSrStreamUpload   = onSR srStreamUpload
    , comCitrixXenclientStorehouseSrStreamClose    = \s -> onSR (\sr -> srStreamClose sr s)
    , comCitrixXenclientStorehouseSrNodeGetChildren = undefined
    , comCitrixXenclientStorehouseSrNodeGetParent = undefined
    , comCitrixXenclientStorehouseSrNodeDelete = undefined
    , comCitrixXenclientStorehouseSrNodeAddKey = \n k -> onSR (\sr -> srSetKey sr n k)
    , comCitrixXenclientStorehouseSrNodeGetKey = \n   -> onSR (\sr -> srGetKey sr n)
    , comCitrixXenclientStorehouseSrNodeDelKey = \n   -> onSR (\sr -> srDelKey sr n)
	}
    where onSR f = liftIO (ctxGetSr ctx uuid >>= f)
          create sr sz = do
              vdiuuid <- srCreateVdi sr sz
              ctxAddVdi ctx (vdiuuid, uuid)
              return vdiuuid
