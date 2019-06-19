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

module Settings ( Settings(..),
                  IncomingChannel(..),
                  OutgoingChannel(..),
                  Port,
                  DomainAddr(..),
                  getSettings )
    where

import Data.String
import System
import System.Exit
import System.FilePath
import System.Console.GetOpt
import System.IO.Unsafe

import Tools.Misc
import Types

data Settings = Settings { incomingChannel :: IncomingChannel
                         , incomingForcedDomID :: Maybe DomID
                         , agentServiceName :: Maybe String
                         , outgoingChannel :: OutgoingChannel
                         , verbose :: Bool
                         , useFirewall :: Bool
                         , runDbusServer :: Bool
                         , authDomainUuid :: Bool
                         , rulesPath :: FilePath
                         , jsonIn :: Bool
                         , jsonOut :: Bool
                         , websocketsIn :: Bool
                         , autoAuth :: Bool
                         , interceptGetOwnerName :: Bool 
                         , interceptAnonymousDestinations :: Bool }

data ChannelFormat = Json | Raw

data IncomingChannel = FromUnixSocket FilePath
                     | FromArgo Port
                     | FromTCP Port
                     | FromSerial FilePath

data OutgoingChannel = ToUnixSocket FilePath
                     | ToArgo Port DomainAddr
                     | ToSerial FilePath

data DomainAddr = ByID DomID
                | ByUuid Uuid

type Port = Int

defaultSettings :: Settings
defaultSettings = Settings { incomingChannel = FromArgo 5555
                           , incomingForcedDomID = Nothing
                           , agentServiceName = Nothing
                           , outgoingChannel = ToUnixSocket "/var/run/dbus/system_bus_socket"
                           , verbose = False
                           , useFirewall = True
                           , runDbusServer = False
                           , authDomainUuid = False
                           , rulesPath = "/etc/rpc-proxy.rules"
                           , jsonIn = False
                           , jsonOut = False
                           , websocketsIn = False
                           , autoAuth = False
                           , interceptGetOwnerName = False
                           , interceptAnonymousDestinations = False }

type Flag = (Settings -> Settings)

parseIncomingChannelConf :: String -> IncomingChannel
parseIncomingChannelConf s =
    case split ':' s of
      ["unix", file] -> FromUnixSocket file
      ["argo", port_str] -> FromArgo (read port_str)
      ["tcp", port_str] -> FromTCP (read port_str)
      ["serial", serial_path] -> FromSerial serial_path
      _ -> error "expected incoming channel spec is unix:file, argo:port, tcp:port, or serial:device_path"

parseOutgoingChannelConf :: String -> OutgoingChannel
parseOutgoingChannelConf s =
    case split ':' s of
      ["unix", file]                     -> ToUnixSocket file
      ["serial", serial_path]            -> ToSerial serial_path
      ["argo-domid", port_str, domid_str] -> ToArgo (read port_str) (ByID $ read domid_str)
      ["argo-uuid", port_str, uuid_str]   -> ToArgo (read port_str) (ByUuid $ fromString uuid_str)
      _ -> error "expected outgoing channel spec is unix:file, argo-domid:port:domid or argo-uuid:port:uuid"

options :: [OptDescr Flag]
options =
    [ Option "il" ["incoming-channel","listen-channel"]
                 (ReqArg (\x s -> s { incomingChannel = parseIncomingChannelConf x }) "CHANNEL")
                 "listen for dbus connections on given channel (unix:file, argo:port, tcp:port, or serial:device_path)"
    , Option "of" ["outgoing-channel","forward-channel"]
                 (ReqArg (\x s -> s { outgoingChannel = parseOutgoingChannelConf x }) "CHANNEL")
                 "forward dbus connections to given channel (unix:file, argo-domid:port:domid, argo-uuid:port:uuid or serial:device_path)"
    , Option "d" ["incoming-domid"]
             (ReqArg (\x s -> s { incomingForcedDomID = Just (read x) }) "DOMID")
             "force the identity of incoming connections to be of given DOMID"
    , Option "v" ["verbose"]
                 (NoArg ( \s -> s { verbose = True } ))
                 "verbose log including all forwarded messages"
    , Option "r" ["rules"]
                 (ReqArg (\x s -> s { rulesPath = x}) "path")
                 "location of rules file"
    , Option "n" ["request-name"]
                 (ReqArg (\x s -> s { agentServiceName = Just x }) "NAME")
                 "replace all request name messages with given name"
    , Option "s" ["server"]
                 (NoArg (\ s -> s { runDbusServer = True }))
                 "run a dbus server for handling call validation requests"
    , Option "w" ["no-firewall"]
                 (NoArg (\ s -> s { useFirewall = False }))
                 "don't do firewall checks for incoming messages"
    , Option "u" ["uuid"]
                 (NoArg (\s -> s { authDomainUuid = True }))
                 "send domain UUID during authentication process in DATA segment"
    , Option ""  ["owner-name"]
                 (NoArg (\ s -> s { interceptGetOwnerName = True }))
                 "intercept GetOwnerName requests and return well-known name"
    , Option ""  ["translate-anonymous-dests"]
                 (NoArg (\ s -> s { interceptAnonymousDestinations = True }))
                 "intercept anonymous destinations and replace with well-known names"
    , Option "" ["json-in"]
                (NoArg (\s -> s { jsonIn = True }))
                "expect json messages as input"
    , Option "" ["json-out"]
                (NoArg (\s -> s { jsonOut = True }))
                "expect json messages as output"
    , Option "" ["websockets-in"]
                (NoArg (\s -> s { websocketsIn = True }))
                "use websockets protocol for incoming connections"

    , Option "" ["auto-auth"]
                (NoArg (\s -> s { autoAuth = True }))
                "automatically authenticate on behalf of connecting client"
    , Option "h" ["help"]
                 (NoArg (\ s -> usage `seq` s))
                 "print usage information"
    ]

usage = unsafePerformIO $
        do prg <- getProgName
           putStrLn (usageInfo prg options)
           exitSuccess

-- Later flags override earlier flags.
getSettings :: [String] -> Settings
getSettings args = let (flags,_,_) = getOpt Permute options args in
                   foldl (flip ($)) defaultSettings flags


