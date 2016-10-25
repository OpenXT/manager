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

module Vm.DomainCore
    ( XSPath
    , getDomainID
    , getStubDomainID
    , updateStubDomainID
    , getDomainUuid
    , whenDomainID
    , whenDomainID_
    , domainXSPath
    , domain0uuid
    ) where

import Data.String
import Data.Maybe
import Data.Int
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Text.Printf
import Vm.Types
import qualified XenMgr.Connect.Xl as Xl
import Tools.XenStore
import XenMgr.Db
import XenMgr.Rpc
import System.Timeout
import Tools.Log

type XSPath = String

domainXSPath :: DomainID -> XSPath
domainXSPath domid = printf "/local/domain/%d" domid

domain0uuid :: Uuid
domain0uuid = fromString "00000000-0000-0000-0000-000000000000"

-- Query xenvm for domain ID
getDomainID :: (MonadRpc e m) => Uuid -> m (Maybe DomainID)
getDomainID uuid
    | uuid == domain0uuid = return (Just 0)
    | otherwise = do
        in_db <- dbExists $ "/vm/" ++ show uuid
        if not in_db
           then return Nothing
           else liftIO $ do
             domid <- Xl.getDomainId uuid
             case domid of
               "" -> return Nothing
               _  -> return (Just (read domid :: Int32))

whenDomainID :: (MonadRpc e m) => a -> Uuid -> (DomainID -> m a) -> m a
whenDomainID def uuid f
    = go =<< getDomainID uuid where
      go Nothing = return def
      go (Just domid) = f domid

whenDomainID_ :: (MonadRpc e m) => Uuid -> (DomainID -> m ()) -> m ()
whenDomainID_ = whenDomainID ()

getDomainUuid :: (MonadIO m, MonadRpc e m) => DomainID -> m (Maybe Uuid)
getDomainUuid domid = do
  v <- liftIO . xsRead $ domainXSPath domid ++ "/vm"
  case v of
    Just ('/':'v':'m':'/':uuid_str) -> return . Just . fromString $ uuid_str
    _ -> return Nothing

stubdomidPath :: Uuid -> String
stubdomidPath uuid = "/xenmgr/vms/" ++ show uuid ++ "/stubdomid"

getStubDomainID :: Uuid -> Rpc (Maybe DomainID)
getStubDomainID uuid = fmap read <$> (liftIO $ xsRead (stubdomidPath uuid))

updateStubDomainID :: Uuid -> Rpc ()
updateStubDomainID uuid = whenDomainID_ uuid $ \domid -> do
  stubdomid <- liftIO $ waitForStubDomainID domid
  case stubdomid of
    Just id -> liftIO $ xsWrite ("/xenmgr/vms/" ++ show uuid ++ "/stubdomid") (show id)
    Nothing -> return ()

-- apparently taking snapshots of vhds can take ages and we have to factor
-- that into the timeout
stubdomIDTimeoutSecs = 3600

-- Query for stub domain ID, waiting for it to appear if necessary
waitForStubDomainID :: DomainID -> IO (Maybe DomainID)
waitForStubDomainID domid
    = do stub_domid <- liftIO . timeout (10^6 * stubdomIDTimeoutSecs) $ xsWaitFor' (stubdomid_path domid) (grab domid)
         when (isNothing stub_domid) $
              warn "TIMEOUT waiting for stub domain ID to appear"
         case stub_domid of
           Just (-1) -> return Nothing
           other -> return other
    where
      stubdomid_path domid = (domainXSPath domid ++ "/image/device-model-domid")
      grab domid = do
        exists <- xsRead (domainXSPath domid)
        if (exists == Nothing)
          then do
            -- the domain evaporated while we were waiting
            warn $ "domain " ++ show domid ++ " disappeared while waiting for stubdom's domid"
            return (Just (-1))
          else fmap read <$> xsRead (stubdomid_path domid)
