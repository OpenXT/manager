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

module StoreHouse.Ctx
	( Ctx
	, ctxNew
	, ctxGetVdiUuids
	, ctxGetSrUuids
	, ctxAddSr
	, ctxAddVdi
	, ctxAddTap
	, ctxGetVdiSr
	, ctxGetSr
	, ctxGetTap
	, ctxRemoveTap
	) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar
import Control.Monad (liftM2)
import Data.Data
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import StoreHouse.SR
import Tools.Uuid
import Tools.TapControl (DeviceMinorNumber)
import qualified Tools.TapControl as Tap
import Tools.UuidTable (UuidTable)
import qualified Tools.UuidTable as Table

type TapDesc = (FilePath, Tap.DeviceMinorNumber)

data VDI = VDI
    { vdiGetSR :: Uuid KindSR
    } deriving (Show,Eq)

data Node = Node Int
    deriving (Show,Eq)

data Ctx = Ctx
    { srs  :: UuidTable KindSR SR
    , vdis :: UuidTable KindVDI VDI -- from vdi uuid to sr uuid.
    , vhds :: UuidTable KindVHD ()
    , taps :: MVar (M.Map FilePath TapDesc)
    }

ctxNew :: IO Ctx
ctxNew = Ctx <$> Table.empty
             <*> Table.empty
             <*> Table.empty
             <*> newMVar M.empty

ctxGetVdiUuids = Table.keys . vdis
ctxGetSrUuids = Table.keys . srs

ctxAddSr ctx (uuid, sr) = Table.insert uuid sr (srs ctx)
ctxAddVdi ctx (uuid, sruuid) = Table.insert uuid (VDI sruuid) (vdis ctx)

ctxAddTap :: Ctx -> (FilePath, (FilePath, DeviceMinorNumber)) -> IO ()
ctxAddTap ctx (filepath, tap) = modifyMVar_ (taps ctx) (return . M.insert filepath tap)

ctxGetTap :: Ctx -> FilePath -> IO (Maybe TapDesc)
ctxGetTap ctx filepath = M.lookup filepath <$> readMVar (taps ctx)

ctxRemoveTap :: Ctx -> FilePath -> IO ()
ctxRemoveTap ctx filepath = modifyMVar_ (taps ctx) (return . M.delete filepath)

-- | returns the SR linked to a specific VDI
ctxGetVdiSr ctx uuid = do
    srUuid <- maybe (error "vdi doesn't exists") vdiGetSR <$> Table.lookup uuid (vdis ctx)
    ctxGetSr ctx srUuid

-- | returns an error if the uuid doesn't exist, otherwise the sr object
ctxGetSr ctx uuid = fromMaybe (error "sr uuid doesn't exists") <$> Table.lookup uuid (srs ctx)
