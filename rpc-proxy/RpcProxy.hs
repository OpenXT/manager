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

{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, PatternGuards, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TupleSections, OverlappingInstances, ViewPatterns #-}
module RpcProxy ( module Control.Monad
                , module Rpc.Core
                , proxy
                ) where

import Data.String ( fromString )
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Concurrent.Lifted
import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Trans
import Control.Applicative
import Data.IORef
import Data.Char
import Data.Maybe
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Control.Exception as E
import qualified Data.HashTable as H
import Data.HashTable (HashTable)
import System.IO
import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import Text.Printf

import DBus.Wire ( marshalMessage, unmarshalMessage, Endianness(..) )
import DBus.Message ( Message(..), ReceivedMessage(..), Serial, receivedSerial, receivedSender
                    , MethodCall(..),Signal(..),MethodReturn(..),Error(..), firstSerial, nextSerial
                    , serialValue )
import DBus.Types ( InterfaceName, MemberName )
import qualified DBus.Types
import Rpc.Core
import Rpc.Autogen.DbusClient

import Tools.IfM
import Tools.Log
import Tools.XenStore
import Tools.Serial

import Settings
import Rules
import Types
import Domain
import Channel
import Firewall
import Bouncer
import Msg.DBus
import Msg.Json
import RpcProxyM
import RulesCache

data Client = Client !Channel !ArtefactSource

instance Show Client where
    show (Client _ s) = show (sourceDomainID s)

data InOutTransforms
   = InOutTransforms Transform Transform

instance Monoid InOutTransforms where
  mempty = InOutTransforms mempty mempty
  (InOutTransforms a b) `mappend` (InOutTransforms p q) =
      InOutTransforms (a `mappend` p) (b `mappend` q)

proxy :: RulesCache -> Settings -> IO ()
proxy rulesCache settings = do
  info "starting DBUS bouncer"
  (dispatcher, _) <- connectBus =<< systemBus
  anonymous_xf <- if interceptAnonymousDestinations settings
                     then replaceAnonymousXF =<< rpcMkContext dispatcher
                     else return mempty
  serverSock <- makeIncomingTransport (incomingChannel settings)

  debug $ "incoming transport: " ++ show serverSock
  -- we count forwarders for debug purposes within counter mvar
  counter <- newMVar 0
  incoming dispatcher counter serverSock anonymous_xf
  where
    accept_incoming :: Channel -> IO (Channel, ArtefactSource)
    accept_incoming transport =
        do (client_socket, inc_domid) <- accept transport
           let domid = fromMaybe inc_domid (incomingForcedDomID settings)
           (,) <$> websockets client_socket <*> mkSource settings domid

    incoming :: Dispatcher -> MVar Int -> Channel -> InOutTransforms -> IO ()
    -- talk over non socket fd
    incoming dispatcher counter serverSock@(FdChann _ _) anonymous_xf =
        do (clientSock, artefact_source) <- accept_incoming serverSock
           -- talk
           conversation dispatcher counter settings rulesCache (clientSock,artefact_source) anonymous_xf
           -- just quit rpc proxy after conversation is finished or error happens
    -- talk over socket
    incoming dispatcher counter serverSock anonymous_xf =
      go =<< E.try (accept_incoming serverSock) where
      go (Right (clientSock, artefact_source)) = do
           -- talk
           forkIO $ conversation dispatcher counter settings rulesCache (clientSock,artefact_source) anonymous_xf
           -- accept new
           incoming dispatcher counter serverSock anonymous_xf
      go (Left (err :: E.SomeException)) = do
        warn $ "problem setting up conversation: " ++ show err
        -- back to accept loop
        incoming dispatcher counter serverSock anonymous_xf

    websockets ch
      | not (websocketsIn settings) = return ch
      | otherwise = wrapInWebSocket ch

    -- HACK: if we are not in firewall mode, don't resolve UUIDs/stubdom target
    -- using xenstore, because we typically might be running in domain which doesn't have
    -- xenstore support. Just assume them false
    mkSource :: Settings -> DomID -> IO ArtefactSource
    mkSource settings domid
      = ArtefactSource domid <$> uuid <*> stubdom where
        fire = useFirewall settings
        uuid | fire = uuidOfDomid domid
             | otherwise = pure Nothing
        stubdom | fire = isJust <$> stubdomTargetDomID domid
                | otherwise = pure False


currentDomainSource :: ArtefactSource
currentDomainSource = ArtefactSource currentDomain Nothing False

newtype JConvM a = JConvM { unJC :: JConvT RpcProxy a }
                   deriving (Functor, Monad, MonadIO)

runJConvM :: RpcContext -> JConvContext RpcProxy -> JConvM a -> IO (Either RpError a)
runJConvM c jc = runRpcProxyM c . runJConvT jc . unJC

instance MsgConvert JConvM Msg JMsg where msgconvert = liftMsgConvertBase JConvM
instance MsgConvert JConvM JMsg Msg where msgconvert = liftMsgConvertBase JConvM

reportErr :: (MonadIO m, Show x) => m (Either x ()) -> m ()
reportErr act = act >>= cont where
  cont (Left x) = warn (show x)
  cont _ = return ()

conversation :: Dispatcher -> MVar Int -> Settings -> RulesCache -> (Channel,ArtefactSource) -> InOutTransforms -> IO ()
conversation dispatcher counter settings rulesCache (client_ch,client_source) anonymous_xf =
  converse `E.catch` (\ex -> close client_ch >> E.throw (ex :: E.SomeException)) where
  converse = do
    -- websockets at least requires handshake
    handshake client_ch
    (forward_ch, forward_domid) <- if autoAuth settings
      then do
        info "automatic auth on, setting up conversation"
        (forward_ch, forward_domid) <-
          makeOutgoingTransport (outgoingChannel settings)
        autoAuthenticate (client_ch,client_source) forward_ch
        return (forward_ch, forward_domid)
      else do
        -- read null byte from client, which signals the conversation start
        null_buf <- recv client_ch 1
        when ( BS.head null_buf /= 0 )
          $ E.throw (AuthError client_ch "expected null byte")
        debug "null byte received from incoming transport - setting up conversation"
        (forward_ch, forward_domid) <-
          makeOutgoingTransport (outgoingChannel settings)
        debug $ "outgoing transport: " ++ show forward_ch
        info  $ "authenticating client " ++ show client_ch ++ " " ++ show client_source
        manualAuthenticate (client_ch,client_source) forward_ch (authDomainUuid settings)
        --
        debug $ printf "auth done for %s" (show client_ch)
        return (forward_ch, forward_domid)

    -- message transform pipeline (includes firewall & some specific message spoofing)
    InOutTransforms xin xout <-
      messageTransforms dispatcher settings rulesCache anonymous_xf (client_ch, client_source) (forward_ch, forward_domid)

    let forwardI r = forward counter r xin
        forwardO r = forward counter r xout
        forwardTwoWays toIO r =
          do forkIO . toIO $ forwardI r
             toIO . forwardO $ reverseRoute r

    -- FIXME: avoid explosion of cases?
    case (jsonIn settings, jsonOut settings) of
      (False, False) ->
        do let route = Route client_ch forward_ch :: Route Msg Msg
           forwardTwoWays id route
      (True, False) ->
        do let route = Route client_ch forward_ch :: Route JMsg Msg
           (rc,jc) <- jconvContext
           either (error . show)
             (\jc -> forwardTwoWays (reportErr . runJConvM rc jc) route)
             jc
      (False, True) ->
        do let route = Route client_ch forward_ch :: Route Msg JMsg
           (rc,jc) <- jconvContext
           either (error . show)
             (\jc -> forwardTwoWays (reportErr . runJConvM rc jc) route)
             jc
      (True, True) ->
        do let route = Route client_ch forward_ch :: Route JMsg JMsg
           (rc,jc) <- jconvContext
           either (error . show)
             (\jc -> forwardTwoWays (reportErr . runJConvM rc jc) route)
             jc

  jconvContext
      = do rc <- rpcMkContext dispatcher
           (rc,) <$> runRpcProxyM rc newJConvContext

authReadLine :: Channel -> IO ByteString
authReadLine s
  = aux BS.empty
  where
    aux buf | end `BS.isSuffixOf` buf = return buf
            | BS.length buf > 128     = E.throw (AuthError s "line length exceeds 128 characters, exploit attempt?")
            | otherwise               =
                recv s 1 >>= \charBuf ->
                if BS.null charBuf
                  then E.throw (EOF s)
                  else aux (BS.append buf charBuf)
    end = BS.pack [ 13, 10 ] -- \r\n

authHexEncode = foldr ((++) . printf "%02x" . ord) []

authMethodDomID :: DomID -> ByteString
authMethodDomID 0 = "AUTH EXTERNAL 30\r\n"
authMethodDomID n = UTF8.fromString $ "AUTH DOMID " ++ authHexEncode (show n) ++ "\r\n"

-- Before the messages start flowing, there is a lame line based authentication protocol
-- we have to forward these without interpreting
manualAuthenticate :: (Channel,ArtefactSource) -> Channel -> Bool -> IO ()
manualAuthenticate (client, ArtefactSource client_domid client_uuid _) server auth_domain_uuid =
    -- null byte at start
    ( do send_all server $ BS.pack [0]
         debug "auth: null byte sent to server"
         loop False
    ) `E.catch` shutdown
  where
    shutdown :: E.SomeException -> IO ()
    shutdown ex = close server >> close client >> E.throw ex

    loop :: Bool -> IO ()
    loop acked =
        do query' <- authReadLine client
           if (client_domid /= 0) &&
              not (isPrefixOf ("AUTH ANONYMOUS" :: ByteString) query') &&
              not (isPrefixOf ("AUTH EXTERNAL" :: ByteString) query') &&
              not acked
              then do send_all client (UTF8.fromString "REJECTED ANONYMOUS\r\n")
                      loop False
              else do let query = auth_swap (toString query')
                      send_all server query
                      case (acked, toString query) of
                        (False, "BEGIN\r\n") -> E.throw (AuthError client "BEGIN without ack from server") -- they're trying to hack us
                        (True , "BEGIN\r\n") -> return () -- we're authenticated
                        _ -> respond
    respond = authReadLine server >>= \response ->
              case toString response of
                'O' : 'K' : _ -> do case (auth_domain_uuid, client_uuid) of
                                        -- send vm UUID as DATA
                                        (True, Just uuid) -> () <$ send client (UTF8.fromString $ "DATA " ++ show uuid ++ "\r\n")
                                        _ -> return ()
                                    send_all client response
                                    loop True -- server authenticated us
                _             -> send_all client response >> loop False -- still talking

    auth_swap x@('A':'U':'T':'H':_) = authMethodDomID client_domid
    auth_swap x =
        UTF8.fromString x -- no change for non auth commands

    toString :: ByteString -> String
    toString b | BS.null b = ""
               | otherwise = case BS.head b of
                               c | c >=0 && c <= 127 -> chr (fromIntegral c) : toString (BS.tail b)
                               _                     -> E.throw (AuthError client "non ASCII character during auth")

autoAuthenticate :: (Channel, ArtefactSource) -> Channel -> IO ()
autoAuthenticate (client, ArtefactSource client_domid client_uuid _) server
  = ( do send_all server $ BS.pack [0]
         debug "auto-auth: null byte sent to server"
         send_all server $ authMethodDomID client_domid
         authReadLine server
         send_all server "BEGIN\r\n"
         return ()
    ) `E.catch` shutdown
  where
    shutdown :: E.SomeException -> IO ()
    shutdown ex = close server >> close client >> E.throw ex


-- | unidirectional message forwarder
forward :: ( MsgConvert m a Msg
           , MsgConvert m Msg b
           , MonadIO m )
           =>
              MVar Int
           -> Route a b
           -> Transform
           -> m ()
forward counter route@(Route from to) xform =
    do debug $ printf "starting forwarding %s" route_str
       liftIO $ incCounter counter
       bounce $ Bouncer route xform
       liftIO stop
  where
    route_str :: String
    route_str = printf "%s -> %s" (show from) (show to)

    stop_because :: E.SomeException -> IO ()
    stop_because err =
        do warn $ printf "stopping because of %s" (show err)
           stop

    stop =
        do debug $ printf "stopping forwarding %s" route_str
           shutdownRecv to `E.catch` ignore
           close from `E.catch` ignore
           decCounter counter

    ignore :: E.SomeException -> IO ()
    ignore err = () <$ warn ("problem during close: " ++ show err)

    incCounter :: MVar Int -> IO ()
    incCounter v = modifyMVar_ v $ \((1+) -> next) ->
        next <$ debug (printf "forked a forwarder, current count: %d" next)

    decCounter :: MVar Int -> IO ()
    decCounter v = modifyMVar_ v $ \(subtract 1 -> next) ->
        next <$ debug (printf "collected a forwarder, current count: %d" next)

-- | incoming/outgoing message transformation pipeline based on settings
messageTransforms ::
  Dispatcher
  -> Settings
  -> RulesCache
  -> InOutTransforms
  -> (Channel, ArtefactSource)
  -> (Channel, DomID)
  -> IO InOutTransforms
messageTransforms dispatcher settings rules anonymous_xf (client_ch, client_src) (forward_ch, forward_domid)
  = do rpc_ctx <- rpcMkContext dispatcher
       mconcat <$> sequence [ pure anonymous_xf -- replace anonymous destinations. has to be before firewall.
                            , fire
                            , pure change_req_name
                            , change_name_owner ]
  where
    change_name_owner
      | interceptGetOwnerName settings = changeNameOwnerXF
      | otherwise = return mempty
    change_req_name
      = InOutTransforms (changeRequestNameXF (agentServiceName settings) client_src)
                        mempty
    fire = InOutTransforms <$> firewall_inc <*> firewall_out
    firewall_inc = firewallXF <$> createFirewall dispatcher
                     FirewallConfig { fireActive = useFirewall settings
                                    , fireRules = rules
                                    , fireVerbose = verbose settings
                                    , fireDirection = Incoming
                                    , fireSource = client_src
                                    , fireDestination = forward_domid }
    firewall_out = firewallXF <$> createFirewall dispatcher
                     FirewallConfig { fireActive = useFirewall settings
                                    , fireRules = rules
                                    , fireVerbose = verbose settings
                                    , fireDirection = Outgoing
                                    , fireSource = currentDomainSource
                                    , fireDestination = sourceDomainID client_src }

-- | drops disallowed messages
firewallXF :: Firewall -> Transform
firewallXF fire = Transform $ \m ->
    ifM (lift $ fire m) (return m) dropMsg

newtype NameTable = NameTable (MVar (Map String String))

newNameTable :: IO NameTable
newNameTable = NameTable <$> newMVar Map.empty

lookupName :: NameTable -> String -> IO (Maybe String)
lookupName (NameTable mv) k = withMVar mv (return . Map.lookup k)

updateName :: NameTable -> String -> String -> IO ()
updateName (NameTable mv) k v = modifyMVar_ mv (return . Map.insert k v)

deleteName :: NameTable -> String -> IO ()
deleteName (NameTable mv) k = modifyMVar_ mv (return . Map.delete k)

queryNameTable :: RpcProxy NameTable
queryNameTable = do
  t <- liftIO newNameTable
  names <- orgFreedesktopDBusListNames "org.freedesktop.DBus" "/org/freedesktop/DBus"
  mapM_ (handleName t) names
  return t
  where
    handleName t (':':_) = return () -- anonymous
    handleName t n = (do
      owner <- orgFreedesktopDBusGetNameOwner "org.freedesktop.DBus" "/org/freedesktop/DBus" n
      liftIO (updateName t owner n) -- 1 owner per name assumed
      ) `catchError` (\_ -> return ())

queryUpdateNameTable ::  NameTable -> RpcProxy ()
queryUpdateNameTable (NameTable mv) = modifyMVar_ mv $ \t -> do
  NameTable mv' <- queryNameTable
  t' <- readMVar mv'
  return (Map.union t' t)

-- replace incoming anynmous destinations (such as :1.15) with well known ones (such as com.citrix.xenclient.xenmgr)
-- required for meaningful firewalling for some "too intelligent" dbus bindings
replaceAnonymousXF context = do
  names <- newNameTable
  -- don't do passive tracking for now to (possibly) save cpu cycles
  -- runRpcProxyM context $ rpcOnSignalFrom dbus (fromString "NameOwnerChanged") (liftIO . handleOwnerChanged names)
  go names
  where
    dbus = Proxy (RemoteObject (fromString "org.freedesktop.DBus") (fromString "/org/freedesktop/DBus")) (fromString "org.freedesktop.DBus")

    handleOwnerChanged names sig =
        let fv = fromVariant in
        case signalArgs sig of
          [ vName, vPrevOwner, vOwner ]
            | Just name <- fv vName
            , Just prevowner <- fv vPrevOwner
            , Just owner <- fv vOwner
            , not (null name), head name /= ':'
              -> liftIO $
                  do when (not $ null prevowner) $ deleteName names prevowner
                     when (not $ null owner) $ updateName names owner name
          _ -> return () 

    go names = return $ InOutTransforms transformIn (Transform return) where
      transformIn = Transform $ \m@(Msg rm buf) ->
        case rm of
          ReceivedMethodCall serial _ call
            | Just dest <- DBus.Message.methodCallDestination call
            , destStr <- DBus.Types.strBusName dest
            , not (TL.null destStr), TL.index destStr 0 == ':' -> do
              let d = TL.unpack destStr
              liftIO $ whenM (isNothing <$> lookupName names d) $ runRpcProxyM context (queryUpdateNameTable names)
              name <- liftIO (lookupName names d)
              case name of
                Nothing   -> warn ("failed to find well known name for " ++ d) >> return m
                Just name -> replaceDestination m name
          _ -> return m
              
      replaceDestination m@(Msg (ReceivedMethodCall serial sender call) buf) new_dest
        = case marshal of
            Right buf  -> return $ Msg rm' (BS.concat $ LazyBS.toChunks buf)
            Left  err  -> warn (show err) >> dropMsg
          where
            rm' = ReceivedMethodCall serial sender (spoofed call)
            marshal = marshalMessage BigEndian serial (spoofed call)
            spoofed call = call { methodCallDestination = Just (fromString new_dest) }
      replaceDestination m _ = return m

-- | Looks for requests to:
-- |
-- | dest=org.freedesktop.DBus
-- | intf=org.freedesktop.DBus
-- | member=GetNameOwner
-- |
-- | Remembers the name being looked up and the request serial number.
-- |
-- | Looks for response with a serial number we remembered and
-- | replaces the answer with the name being looked up.
-- |
-- | Effect of this is to prevent connection names (e.g. :12.34) being
-- | passed to guests. gdbus (libgio) calls GetNameOwner and uses the returned
-- | connection name for property gets to the service. Sending back the well-known
-- | name instead allows it to proceed.
-- |
-- | It's probably good anyway not to expose dom0 connection names.

changeNameOwnerXF = do
  ownerNameRequests <- H.new (==) H.hashInt
  let transformIn =
        Transform $ \m@(Msg rm buf) ->
          case rm of
            ReceivedMethodCall serial _ call -> catch_call m serial call
            _ -> return m
        where
          catch_call m serial call =
            case () of
              _ | methodCallMember call == memberGetNameOwner
                , methodCallDestination call == Just nameOrgFreedesktopDBus -> do
                    lift $ H.insert ownerNameRequests (fromIntegral (serialValue serial)) (methodCallBody call)
                    return m
                | otherwise -> return m
      transformOut =
        Transform $ \m@(Msg rm buf) ->
          case rm of
            ReceivedMethodReturn serial bus ret -> let
              spoof new_body
                = case marshal of
                    Right buf  -> return $ Msg (ReceivedMethodReturn serial bus new_m) (BS.concat $ LazyBS.toChunks buf)
                    Left  err  -> lift (warn (show err)) >> dropMsg
                  where
                    new_m = MethodReturn { methodReturnSerial = methodReturnSerial ret
                                         , methodReturnDestination = methodReturnDestination ret
                                         , methodReturnBody = new_body }
                    marshal = marshalMessage BigEndian serial new_m

              in do val <- lift $ H.lookup ownerNameRequests retSerial
                    case val of
                      Just body -> do lift $ H.delete ownerNameRequests retSerial
                                      spoof body
                      _ -> return m
                 where retSerial = fromIntegral . serialValue . methodReturnSerial $ ret
            _ -> return m
  return $ InOutTransforms transformIn transformOut

-- | replaces all RequestName messages with name constructed from guest vm uuid
changeRequestNameXF :: Maybe String -> ArtefactSource -> Transform
changeRequestNameXF service (ArtefactSource _ maybe_uuid _)
  = Transform $ \m@(Msg rm buf) ->
      case rm of
        ReceivedMethodCall serial _ call -> change_call m serial call
        _ -> return m
    where
      spoofed call n =
        MethodCall { methodCallPath        = pathOrgFreedesktopDBus
                   , methodCallMember      = memberRequestName
                   , methodCallInterface   = Just intfOrgFreedesktopDBus
                   , methodCallDestination = Just nameOrgFreedesktopDBus
                   , methodCallFlags       = methodCallFlags call
                   , methodCallBody        = replace_name n (methodCallBody call) }
          where replace_name n [name_v, flags_v] = [DBus.Types.toVariant n, flags_v]
                replace_name _ args = args

      spoof m@(Msg rm _) serial call new_name
        = case marshal of
            Right buf  -> return $ Msg rm (BS.concat $ LazyBS.toChunks buf)
            Left  err  -> warn (show err) >> dropMsg
          where
            marshal = marshalMessage BigEndian serial (spoofed call new_name)

      change_call m serial call =
        case () of
          _ | methodCallMember call == memberRequestName
            , methodCallDestination call == Just nameOrgFreedesktopDBus
            , Just uuid <- maybe_uuid
            , Just s <- service            -> spoof m serial call (replace "$UUID" (uuid_underscores uuid) s)
            | otherwise                    -> return m
        where uuid_underscores uuid = replace "-" "_" (show uuid)
              replace pat repl txt = T.unpack $ T.replace (T.pack pat) (T.pack repl) (T.pack txt)

pathOrgFreedesktopDBus = fromString "/org/freedesktop/DBus"
nameOrgFreedesktopDBus = fromString "org.freedesktop.DBus"
intfOrgFreedesktopDBus = fromString "org.freedesktop.DBus"
memberRequestName = fromString "RequestName"
memberGetNameOwner = fromString "GetNameOwner"
memberNameOwnerChanged = fromString "NameOwnerChanged"
