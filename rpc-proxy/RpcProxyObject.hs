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

{-# LANGUAGE RankNTypes #-}
module RpcProxyObject where

import Control.Applicative
import Data.Text.Lazy (Text)
import Data.String
import qualified Data.Text.Lazy as TL
import Data.Int
import Data.Maybe
import Data.IORef
import System.IO
import Rpc.Core
import Rpc.Autogen.RpcProxyServer
import RpcProxy
import RpcProxyM
import Rules
import Types
import Domain
import RulesCache
import Tools.XenStore
import Tools.Log
import qualified Data.Traversable as DT

expose :: RulesCache -> RpcProxy ()
expose rulesCache = rpcExpose (fromString "/") . interfaces . implementation $ test rulesCache

implementation :: (forall a . Artefact a => a -> RuleSubject -> RpcProxy Bool) -> RpcProxyServer RpcProxy
implementation test =
    RpcProxyServer
    { comCitrixXenclientRpcProxyValidateCall = validateCall
    , comCitrixXenclientRpcProxyValidateRecvSignal = validateRecvSignal
    , comCitrixXenclientRpcProxyValidateSendSignal = validateSendSignal
    }
  where
    validateCall :: Int32 -> String -> String -> String -> RpcProxy Bool
    validateCall domid dest intf member =
        validateWithUuid domid descr $ \uuid ->
        test ( CallHeader (ArtefactSource domid (Just uuid) False) (TL.pack dest) (TL.pack intf) (TL.pack member) )
              ( RuleSubject Incoming TagMethodCall )
      where
        descr = "UID method call:" ++ show domid ++ ":" ++ dest ++ ":" ++ intf ++ ":" ++ member

    validateRecvSignal :: Int32 -> String -> String -> RpcProxy Bool
    validateRecvSignal domid intf member =
        validateWithUuid domid descr $ \uuid ->
        test ( SignalHeader (ArtefactSource domid (Just uuid) False) (TL.pack intf) (TL.pack member) )
              ( RuleSubject Outgoing TagSignal )
      where
        descr = "UID incoming signal:" ++ show domid ++ ":" ++ intf ++ ":" ++ member

    validateSendSignal :: Int32 -> String -> String -> RpcProxy Bool
    validateSendSignal domid intf member =
        validateWithUuid domid descr $ \uuid ->
        test ( SignalHeader (ArtefactSource domid (Just uuid) False) (TL.pack intf) (TL.pack member) )
              ( RuleSubject Incoming TagSignal )
      where
        descr = "UID outgoing signal:" ++ show domid ++ ":" ++ intf ++ ":" ++ member

    validateWithUuid domid descr test_f =
        do maybe_uuid <- liftIO $ uuidOfDomid ( fromIntegral domid )
           allow <- fromMaybe False <$> DT.sequence (test_f <$> maybe_uuid)
           unless allow $ liftIO (warn $ "DENY " ++ descr)
           return allow

data CallHeader = CallHeader !ArtefactSource !Text !Text !Text

instance Artefact CallHeader where
    artefactType _ = TagMethodCall
    artefactSource (CallHeader s _ _ _) = s
    artefactSender _ = Nothing
    artefactDestination (CallHeader _ d i m) = Just d
    artefactInterface (CallHeader _ d i m) = Just i
    artefactMember (CallHeader _ d i m) = Just m
    artefactPropertyInterface _ = Nothing
    
data SignalHeader = SignalHeader !ArtefactSource !Text !Text

instance Artefact SignalHeader where
    artefactType _ = TagSignal
    artefactSource (SignalHeader s _ _) = s
    artefactSender _ = Nothing
    artefactDestination _ = Nothing
    artefactInterface (SignalHeader _ i m) = Just i
    artefactMember (SignalHeader _ i m) = Just m
    artefactPropertyInterface _ = Nothing
    
