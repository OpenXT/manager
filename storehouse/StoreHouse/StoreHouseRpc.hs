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

module StoreHouse.StoreHouseRpc where

import Control.Concurrent
import Data.String
import Tools.Log
import Rpc.Autogen.StorehouseServer
import Rpc.Core
import Tools.Error
import StoreHouse.Rpc
import StoreHouse.ObjectPaths

expose :: Rpc ()
expose = rpcExpose storehouseObjectPath (interfaces $ implementation)

implementation :: StorehouseServer Rpc
implementation = StorehouseServer
	{ comCitrixXenclientStorehouseDummyMethod  = \s -> liftIO $ putStrLn ("debug: " ++ s)--debug ("dummy method: " ++ s)
	, comCitrixXenclientStorehouseGetDummyProp = return "dummy val"
	}
