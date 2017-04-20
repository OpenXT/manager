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

module XenMgr.Connect.NetworkDaemon
    ( NetworkInfo(..)
    , listNetworks
    , statNetwork
    , joinNetwork
    , anyNetworksActive
    , onNetworkAdded, onNetworkRemoved, onNetworkStateChanged, onNetworkStateChangedRemove
    , getNetworkBackend'
    , vifConnected
    , netbackToUuid
    ) where

import Data.Time
import Data.String
import Data.Maybe
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.Error (catchError)
import Control.Monad.Trans
import Control.Concurrent

import Vm.Types
import Vm.DmTypes
import Vm.Uuid
import qualified Data.Text.Lazy as TL
import Text.Printf
import Tools.Misc
import Tools.Log
import Tools.Text

import Rpc.Autogen.NetworkDaemonClient
import Rpc.Autogen.NetworkClient
import XenMgr.Rpc
import XenMgr.Errors

service = "com.citrix.xenclient.networkdaemon"
rootS = "/"

vifS :: DomainID -> XbDeviceID -> String
vifS domid (XbDeviceID di) = printf "vif%d.%d" domid di

npathS :: Network -> String
npathS = TL.unpack . strObjectPath . networkObjectPath

ready :: Rpc Bool
ready = comCitrixXenclientNetworkdaemonIsInitialized service rootS

vifConnected :: DomainID -> NicID -> DomainID -> Rpc Bool
vifConnected frontdomid nicid backdomid = comCitrixXenclientNetworkdaemonVifConnected service rootS (vifS frontdomid nicid) backdomid

repeatUntil :: (MonadIO m) => m Bool -> Float -> Float -> m Bool
repeatUntil pred delay to
    = do t0 <- liftIO getCurrentTime
         let test = do
               p  <- pred
               t1 <- liftIO getCurrentTime
               let dt = realToFrac $ diffUTCTime t1 t0
               case () of
                 _ | p         -> return True
                   | t1 < t0   -> return False
                   | dt > to   -> return False
                   | otherwise -> liftIO (threadDelay (round $ 10^6 * to)) >> test
         test

listNetworks :: Rpc [Network]
listNetworks
    = catMaybes . map from_map <$> comCitrixXenclientNetworkdaemonList service rootS
      where from_map m
              = do o <- fromString <$> Map.lookup "object" m
                   return $ networkFromStr o

-- expects that a) daemon is initialised b) network object is exported
-- if not true, waits a bit before retrying and failing
statNetwork :: Network -> Rpc (Maybe NetworkInfo)
statNetwork n
    = stat =<< repeatUntil ready 1 30 where
      stat False = failTimeoutWaitingForNetworkDaemon
      stat True  =
        (rpcRetryOnError 10 250 retryCheck $ (Just <$> statNetwork' n)) `catchError` onErr
      onErr ex = warn ("cannot stat " ++ show n ++ ": " ++ show ex) >> return Nothing
      retryCheck e = case toRemoteErr e of
                       Nothing -> False
                       Just (RemoteErr name _) -> name == fromString "org.freedesktop.DBus.Error.UnknownObject"

statNetwork' :: Network -> Rpc NetworkInfo
statNetwork' n
    = NetworkInfo n
      <$> getNetworkName n
      <*> getNetworkBridgeName n
      <*> (fromString <$> getNetworkBackend n)
      <*> (getNetworkType n >>= \t -> return (t == eNETWORK_TYPE_WIFI || t == eNETWORK_TYPE_MODEM))
      <*> (getNetworkConnection n >>= \t -> return (t == eCONNECTION_TYPE_SHARED))
      <*> (getNetworkType n >>= \t -> return (t == eNETWORK_TYPE_INTERNAL))
      <*> isNetworkConfigured n
      <*> getNetworkCarrier n

getNetworkName = comCitrixXenclientNetworkConfigGetName service . npathS
getNetworkBridgeName = comCitrixXenclientNetworkConfigGetBridge service . npathS
getNetworkBackend = comCitrixXenclientNetworkConfigGetBackendUuid service . npathS
getNetworkBackend' :: Network -> Rpc (Maybe Uuid)
getNetworkBackend' n = make <$> comCitrixXenclientNetworkdaemonGetNetworkBackend service rootS (npathS n) where
  make "" = Nothing
  make s  = Just (fromString s)

getNetworkType = comCitrixXenclientNetworkConfigGetType service . npathS
getNetworkConnection = comCitrixXenclientNetworkConfigGetConnection service . npathS
isNetworkConfigured = comCitrixXenclientNetworkIsConfigured service . npathS
getNetworkCarrier = comCitrixXenclientNetworkConfigGetActive service . npathS

anyNetworksActive :: Rpc Bool
anyNetworksActive = comCitrixXenclientNetworkdaemonIsNetworkingActive service rootS

joinNetwork :: Network -> DomainID -> NicID -> Rpc ()
joinNetwork n domid devid = comCitrixXenclientNetworkdaemonMoveToNetwork service rootS (vifS domid devid) (npathS n)

onNetworkAdded, onNetworkRemoved :: (Network -> Rpc ()) -> Rpc ()
onNetworkAdded f
    = rpcOnSignal rule handle
    where
      rule = matchSignal "com.citrix.xenclient.networkdaemon.notify" "network_added"
      handle _ s =
          let (networkV:_) = signalArgs s in
          case fromVariant networkV of
            Just name -> f $ networkFromStr name
            _ -> return ()

onNetworkRemoved f
    = rpcOnSignal rule handle
    where
      rule = matchSignal "com.citrix.xenclient.networkdaemon.notify" "network_removed"
      handle _ s =
          let (networkV:_) = signalArgs s in
          case fromVariant networkV of
            Just name -> f $ networkFromStr name
            _ -> return ()

onNetworkStateChanged :: (Int -> String -> Rpc ()) -> Rpc ()
onNetworkStateChanged f
    = rpcOnSignal rule handle
    where
      rule = matchSignal "com.citrix.xenclient.networkdaemon.notify" "network_state_changed"
      handle _ s =
          let (networkV:stateV:backendV:_) = signalArgs s in
          case (fromVariant stateV, fromVariant backendV) of
            (Just state, Just backend) -> f (networkStateFromStr state) backend
            (_, _) -> return ()


onNetworkStateChangedRemove :: (Int -> String -> Rpc ()) -> Rpc ()
onNetworkStateChangedRemove f
    = rpcOnSignalRemove rule handle
    where
      rule = matchSignal "com.citrix.xenclient.networkdaemon.notify" "network_state_changed"
      handle _ s =
          let (networkV:stateV:backendV:_) = signalArgs s in
          case (fromVariant stateV, fromVariant backendV) of
            (Just state, Just backend) -> f (networkStateFromStr state) backend
            (_, _) -> return ()

-- "/ndvm/000000000_0000_0000_00000001"
netbackToUuid :: String -> Uuid
netbackToUuid backend = fromString $ replace "_" "-" $ last $ split '/' backend
