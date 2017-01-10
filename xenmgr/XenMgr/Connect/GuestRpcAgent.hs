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

module XenMgr.Connect.GuestRpcAgent
    ( guestAgentRunning
    , shutdown
    , sleep
    , hibernate
    , reboot
    , onAgentStarted
    , onAgentStartedRemove
    , onAgentUninstalled
    , onAgentUninstalledRemove
    , onXorgRunning
    ) where

import Data.String
import Control.Monad
import Tools.Misc
import Vm.Types
import qualified Data.Text.Lazy as TL
import qualified XenMgr.Connect.Xl as Xl
import Rpc.Core
import Rpc.Autogen.GuestClient
import Rpc.Autogen.DbusClient
import XenMgr.Rpc
import XenMgr.Errors
import XenMgr.Host
import Vm.Queries
import Tools.Log
import Tools.Text
import Text.Printf

guestAgentRunning :: Uuid -> Rpc Bool
guestAgentRunning uuid = isServiceRunning (agentService uuid)

agentService :: Uuid -> String
agentService uuid =
    "com.citrix.xenclient.guest.uuid_" ++ (replace "-" "_" $ show uuid)

shutdown :: Uuid -> Rpc ()
shutdown uuid =
    do comCitrixXenclientGuestRequestShutdown (agentService uuid) "/"
       done <- liftIO $ Xl.waitForState uuid Shutdown Nothing
       when (not done) $ failShutdownTimeout

sleep :: Uuid -> Rpc ()
sleep uuid =
    do comCitrixXenclientGuestRequestSleep (agentService uuid) "/"
       done <- liftIO $ Xl.waitForAcpiState uuid 3 (Just 180)
       when (not done) $ failSleepTimeout

hibernate :: Uuid -> Rpc ()
hibernate uuid =
    do comCitrixXenclientGuestRequestHibernate (agentService uuid) "/"
       done <- liftIO $ Xl.waitForState uuid Shutdown (Just 180)
       when (not done) $ failHibernateTimeout

reboot :: Uuid -> Rpc ()
reboot uuid = comCitrixXenclientGuestRequestReboot (agentService uuid) "/"

onAgentStartedRemove :: Uuid -> Rpc () -> Rpc ()
onAgentStartedRemove uuid action =
    let rule = matchSignal "com.citrix.xenclient.guest" "agent_started"
    in
      rpcOnSignalRemove rule process
  where
    process sender_name signal = do
      do let sender = TL.unpack (strBusName sender_name)
         connection_domid <- orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" sender
         expected_domid   <- getDomainID uuid
         case expected_domid of
           Just edomid
               | edomid == connection_domid ->
                   do debug $ printf "received agent_started signal from domain %d (uuid %s)" connection_domid (show uuid)
                      action
           _ -> return ()

onAgentStarted :: Uuid -> Rpc () -> Rpc ()
onAgentStarted uuid action =
    let rule = matchSignal "com.citrix.xenclient.guest" "agent_started"
    in
      rpcOnSignal rule process
  where
    process sender_name signal = do
      do let sender = TL.unpack (strBusName sender_name)
         connection_domid <- orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" sender
         expected_domid   <- getDomainID uuid
         case expected_domid of
           Just edomid
               | edomid == connection_domid ->
                   do debug $ printf "received agent_started signal from domain %d (uuid %s)" connection_domid (show uuid)
                      action
           _ -> return ()

onAgentUninstalledRemove :: Uuid -> Rpc () -> Rpc ()
onAgentUninstalledRemove uuid action =
    let rule = matchSignal "com.citrix.xenclient.guest" "agent_uninstalled"
    in
      rpcOnSignalRemove rule process
  where
    process sender_name signal = do
      do let sender = TL.unpack (strBusName sender_name)
         connection_domid <- orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" sender
         expected_domid   <- getDomainID uuid
         case expected_domid of
           Just edomid
               | edomid == connection_domid ->
                   do debug $ printf "received agent_uninstalled signal from domain %d (uuid %s)" connection_domid (show uuid)
                      action
           _ -> return ()

onAgentUninstalled :: Uuid -> Rpc () -> Rpc ()
onAgentUninstalled uuid action =
    let rule = matchSignal "com.citrix.xenclient.guest" "agent_uninstalled"
    in
      rpcOnSignal rule process
  where
    process sender_name signal = do
      do let sender = TL.unpack (strBusName sender_name)
         connection_domid <- orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" sender
         expected_domid   <- getDomainID uuid
         case expected_domid of
           Just edomid
               | edomid == connection_domid ->
                   do debug $ printf "received agent_uninstalled signal from domain %d (uuid %s)" connection_domid (show uuid)
                      action
           _ -> return ()

onXorgRunning :: (Uuid -> Rpc ()) -> Rpc ()
onXorgRunning action =
    let rule = matchSignal "com.citrix.xenclient.guest" "xorg_running"
    in
      rpcOnSignal rule process
  where
    process sender_name signal = do
      do let sender = TL.unpack (strBusName sender_name)
         connection_domid <- orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" sender
         maybe_uuid <- getDomainUuid connection_domid
         case maybe_uuid of
           Nothing   -> return ()
           Just uuid -> do
             debug $ printf "received xorg_running signal from domain %d (uuid %s)" connection_domid (show uuid)
             action uuid
