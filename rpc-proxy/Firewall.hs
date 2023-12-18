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

{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

module Firewall
       ( FirewallConfig (..)
       , Firewall
       , createFirewall
       ) where

import Data.Maybe
import Data.String
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import qualified Data.Text.Lazy as TL
import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Control.Exception as E
import Text.Printf

import Rules
import RulesCache
import Msg.DBus
import Types
import Tools.Log

import DBus (Serial, receivedMessageSerial, receivedMessageSender)
import DBus.Internal.Message ( Message(..), ReceivedMessage(..)
                    , MethodCall(..),Signal(..),MethodReturn(..),MethodError(..) )
import DBus.Internal.Types ( formatBusName, formatInterfaceName, formatMemberName, toVariant )
import qualified DBus.Internal.Types as DT

import Rpc.Core ( Proxy (..), fromVariant, RemoteObject (..), ObjectPath, remote, Dispatcher, mkObjectPath_, Variable (..) )

type Firewall = Msg -> IO Bool

data FirewallConfig
   = FirewallConfig { fireActive      :: Bool
                    , fireRules       :: RulesCache
                    , fireVerbose     :: Bool
                    , fireDirection   :: Direction
                    , fireSource      :: ArtefactSource
                    , fireDestination :: DomID }

-- The dbus received messages are artefacts we can filter upon
data MsgAndSource = MAS !ArtefactSource !Msg

instance Artefact MsgAndSource where

    artefactType (MAS _ (Msg ReceivedMethodCall {} _)) = TagMethodCall
    artefactType (MAS _ (Msg ReceivedMethodReturn {} _)) = TagMethodReturn
    artefactType (MAS _ (Msg ReceivedSignal {} _)) = TagSignal
    artefactType (MAS _ (Msg ReceivedMethodError {} _)) = TagError
    artefactType (MAS _ (Msg ReceivedUnknown {} _)) = undefined

    artefactSource (MAS src _) = src

    artefactSender (MAS _ (Msg m _)) = fmap formatBusName $ receivedMessageSender m

    artefactDestination (MAS _ (Msg (ReceivedMethodCall _ m) _)) = fmap formatBusName $ methodCallDestination m
    artefactDestination (MAS _ (Msg (ReceivedMethodReturn _ m) _)) = fmap formatBusName $ methodReturnDestination m
    artefactDestination (MAS _ (Msg (ReceivedMethodError _ m) _)) = fmap formatBusName $ methodErrorDestination m
    artefactDestination (MAS _ (Msg (ReceivedSignal _ m) _)) = fmap formatBusName $ signalDestination m
    artefactDestination _ = Nothing

    artefactInterface (MAS _ (Msg (ReceivedMethodCall _ m) _)) = fmap formatInterfaceName $ methodCallInterface m
    artefactInterface (MAS _ (Msg (ReceivedSignal _ m) _)) = Just . formatInterfaceName $ signalInterface m
    artefactInterface _ = Nothing

    artefactMember (MAS _ (Msg (ReceivedMethodCall _ m) _)) = Just . formatMemberName $ methodCallMember m
    artefactMember (MAS _ (Msg (ReceivedSignal _ m) _)) = Just . formatMemberName $ signalMember m
    artefactMember _ = Nothing

    artefactPropertyInterface (MAS _ (Msg (ReceivedMethodCall _ m) _))
     |   methodCallInterface m == Just (fromString "org.freedesktop.DBus.Properties")
       , methodCallMember m `elem` [fromString "Get", fromString "Set", fromString "GetAll"]
       = case methodCallBody m of
          (v : _) -> DT.fromVariant v
          _ -> Nothing
    artefactPropertyInterface _ = Nothing
      
type PropertyMap   = Map (Uuid, String) PropertyValue
data PropertyValue = PV_Bool Bool | PV_String String

type Env = (Dispatcher, Maybe Uuid, MVar PropertyMap)

-- fills additional per-rule data
augmentRule :: Env -> Rule -> IO Rule
augmentRule (_, Nothing, _) rule = return rule
augmentRule (client, Just uuid, pmap) rule@Rule { match = match } = do
    p <- mapM augmentPropertyMatch (properties match)
    return $ rule {match = match { properties = p }}
    where
      augmentPropertyMatch (PropertyMatchB pname expected _)
        = PropertyMatchB pname expected . Just . extrBool <$> getCachedValue pname PV_Bool
      augmentPropertyMatch (PropertyMatchS pname expected _)
        = PropertyMatchS pname expected . Just . extrString <$> getCachedValue pname PV_String
      
      extrBool (PV_Bool v) = v
      extrBool _ = error "boolean expected"
      extrString (PV_String v) = v
      extrString _ = error "string expected"
      
      getCachedValue :: (Show a, Variable a) => String -> (a -> PropertyValue) -> IO PropertyValue
      getCachedValue pname cons
        = modifyMVar pmap $ \pm ->
            case Map.lookup (uuid, pname) pm of
              Just v -> return (pm, v)
              Nothing -> do
                -- query current property value via xenmgr access
                v <- testVm (vmPath uuid) pname
                debug $ "assuming (" ++ show uuid ++ "," ++ pname ++ ") = " ++ show v
                return ( Map.insert (uuid,pname) (cons v) pm
                       , cons v )
      
      testVm :: Variable a => ObjectPath -> String -> IO a
      testVm path =
          liftM (fromMaybe (error "testVm: bad variant") . fromVariant) .
          mustSucceed . remote client (xenmgrVmProxy path) "Get"
                       "com.citrix.xenclient.xenmgr.vm.unrestricted"

createFirewall :: Dispatcher -> FirewallConfig -> IO Firewall
createFirewall client c
  = do pmap <- newMVar Map.empty
       return $ \msg@(Msg rm _) ->
         do let artefact = MAS (fireSource c) msg
            let domid = sourceDomainID (fireSource c)
                uuid  = sourceUuid (fireSource c)
                em    = "<none>"
                typ   = artefactType artefact
                sndr  = fromMaybe em $ artefactSender artefact -- why this is usually null
                dest  = fromMaybe em $ artefactDestination artefact
                intf  = fromMaybe em $ artefactInterface artefact
                memb  = fromMaybe em $ artefactMember artefact
                reply_serial = case rm of
                  ( ReceivedMethodReturn _ m ) -> Just $ methodReturnSerial m
                  ( ReceivedMethodError _ m ) -> Just $ methodErrorSerial m
                  _ -> Nothing
                description rulet =
                  printf "(%d->%d) %s %s { sender='%s' dest='%s' intf='%s' member='%s' serial='%s' reply-to='%s' }"
                            domid (fireDestination c) rulet (show typ) sndr dest intf memb
                            (show $ receivedMessageSerial rm) (show reply_serial)

            access <- if fireActive c && domid /= 0
                      then testAugmented (fireRules c) (augmentRule (client,uuid,pmap))
                           artefact (subjectFor (fireDirection c) rm)
                      else return True
            unless access $ warn $ description "DENY"
            when (fireVerbose c && access) $ info (description "ALLOW")
            return access

subjectFor :: Direction -> ReceivedMessage -> RuleSubject
subjectFor d ReceivedMethodCall {}   = RuleSubject d TagMethodCall
subjectFor d ReceivedSignal {}       = RuleSubject d TagSignal
subjectFor d ReceivedMethodReturn {} = RuleSubject d TagMethodReturn
subjectFor d ReceivedMethodError {}  = RuleSubject d TagError
subjectFor d _                       = RuleSubject d TagAny

xenmgrProxy :: Proxy
xenmgrProxy =
    Proxy obj (fromString "com.citrix.xenclient.xenmgr")
  where
    obj = RemoteObject (fromString "com.citrix.xenclient.xenmgr") (fromString "/")

xenmgrVmProxy :: ObjectPath -> Proxy
xenmgrVmProxy objp =
    Proxy obj (fromString "org.freedesktop.DBus.Properties")
    where
      obj = RemoteObject (fromString "com.citrix.xenclient.xenmgr") objp

vmPath :: Uuid -> ObjectPath
vmPath uuid
  = mkObjectPath_  $ TL.unpack $ fromString "/vm/" `TL.append` TL.map replaceMinus (uuidText uuid)
    where
      replaceMinus '-' = '_'
      replaceMinus x   = x

mustSucceed :: IO a -> IO a
mustSucceed action =
    action `E.catch` err
    where
      err (e :: E.SomeException) = warn ("error: " ++ show e) >> threadDelay (10^6) >> mustSucceed action
