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

{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module Msg.Json
       ( JMsg (..)
       , jsonMessages
       , JConvContext
       , JConvT(..)
       , newJConvContext
       , runJConvT
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader

import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.List
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Text.Lazy as TL
import qualified Control.Exception as E
import Text.JSON
import Text.Printf
import System.IO.Unsafe

import DBus.Types hiding (fromVariant,toVariant)
import qualified DBus.Types as DB
import DBus.Message
import DBus.Wire ( marshalMessage, unmarshalMessage, Endianness(..) )

import Rpc.Core hiding (ObjectPath, BusName, MemberName, InterfaceName, mkObjectPath_, mkBusName_, mkMemberName_, mkInterfaceName_) --hiding (ObjectPath, BusName, MemberName, InterfaceName)
import qualified Rpc.Core as Rpc
import Channel
import Msg.JsonRpc
import Msg.JsonConv
import Msg.DBus

import Bouncer

import Tools.Log
import Tools.FreezeIOM

import Control.Concurrent
import qualified Control.Monad.Error as CME
import Data.String
import qualified Data.Map as M

import qualified DBus.Introspection as I

data JMsg
   = JMsgReq JReq
   | JMsgSignal JSignal
   | JMsgResp JResp
   | JMsgRespErr JRespErr

-- special treatment for websocket channels as we do not assume null terminated jsons but
-- we assume messages are framed and boundaries preserved
jsonMessages :: Channel -> IO [ JMsg ]
jsonMessages c@(WebSocketCh{}) = framedJsonMessages c
jsonMessages c = nullSeparatedJsonMessages =<< incomingData c

--  1MB
maxFrameLen = 1024*1024

-- on message boundary preserving channel
framedJsonMessages :: Channel -> IO [ JMsg ]
framedJsonMessages ch
  = frame =<< recv ch maxFrameLen
    where
      frame x
        | B.null x = return [] -- EOF
        | otherwise =
          do m_ <- decodeJMsg (UTF8B.toString x)
             case m_ of
               Nothing -> framedJsonMessages ch
               Just m  -> (:) <$> pure m <*> unsafeInterleaveIO (framedJsonMessages ch)

-- on channel which does not preserve msg boundaries
nullSeparatedJsonMessages :: BL.ByteString -> IO [ JMsg ]
nullSeparatedJsonMessages dat
  = do let (x,xs) = BL.break (== 0) dat
       ys <- if BL.null xs
               then return []
               else unsafeInterleaveIO (nullSeparatedJsonMessages $ BL.tail xs)
       if BL.null x
         then return []
         else do
           m_ <- decodeJMsg (UTF8.toString x)
           case m_ of
             Nothing -> return ys
             Just m  -> return (m:ys)

decodeJMsg str
  = case decode str of
         Ok v ->
           case (jmsgFromJson v) of
             Just m -> return (Just m)
             _ -> warn ("invalid json message: " ++ str) >> return Nothing
         _ -> warn ("json parse failed: " ++ str) >> return Nothing


jmsgFromJson :: JSValue -> Maybe JMsg
jmsgFromJson v@(JSObject o)
  = let kv = fromJSObject o in
  from_type (fromMaybe "request" (join $ fmap unpstr $ "type" `lookup` kv))
  where
    from_type "request"  = JMsgReq <$> jreqFromJson v
    from_type "signal"   = JMsgSignal <$> jsignalFromJson v
    from_type "response" = JMsgResp <$> jrespFromJson v
    from_type "error"    = JMsgRespErr <$> jerrFromJson v
    from_type _          = Nothing

    unpstr (JSString s)  = Just $ fromJSString s
    unpstr _             = Nothing

jmsgFromJson _ = Nothing

bufferSz = 4096
-- Lazy chunks of incoming data, terminating on EOF or error
incomingData :: Channel -> IO BL.ByteString
incomingData sock =
    ( do chunk <- recv sock bufferSz
         when ( B.null chunk ) $ E.throw (EOF sock)
         other_chunks <- unsafeInterleaveIO $ incomingData sock
         return $ BL.append (BL.fromChunks [chunk]) other_chunks
    ) `E.catch` err
  where
    err :: E.SomeException -> IO BL.ByteString
    err x = do _err $ E.fromException x
               return BL.empty
    _err (Just (EOF s)) = do debug $ printf "%s on %s" (show (EOF s)) (show sock)
    _err (Just ex) = do warn $ printf "%s on %s" (show ex) (show sock)
    _err Nothing = do return ()

jmsgToJson :: JMsg -> JSValue
jmsgToJson (JMsgReq m) = jreqToJson m
jmsgToJson (JMsgSignal m) = jsignalToJson m
jmsgToJson (JMsgResp m) = jrespToJson m
jmsgToJson (JMsgRespErr m) = jerrToJson m

data JConvContext m
   = JConvContext
     {
       findSig :: JMsg -> m (Maybe [Type])
     }

data QSig = QSig BusName ObjectPath InterfaceName MemberName

newJConvContext :: (FreezeIOM ctx (Either e) m, MonadRpc e m) => m (JConvContext m)
newJConvContext =
  do f <- sigFinder
     return JConvContext {
       findSig = \m ->
        case m of
          JMsgReq r ->
            let q = QSig <$> Just (mkBusName_ (jreqDest r))
                         <*> Just (mkObjectPath_ (jreqPath r))
                         <*> Just (mkInterfaceName_ (jreqInterface r))
                         <*> Just (mkMemberName_ (jreqMethod r))
            in case q of
              Just (QSig b o i m) -> f b o i m
              _ -> return Nothing
          _ -> return Nothing
        }

newtype
    JConvT m a
  = JConvT { unJConvT :: ReaderT (JConvContext m) m a }
    deriving (Functor, Monad, MonadIO)

instance MonadTrans JConvT where
  lift f = JConvT $ lift f

context :: Monad m => JConvT m (JConvContext m)
context = JConvT $ ask

runJConvT :: (Monad m) => JConvContext m -> JConvT m a -> m a
runJConvT c f
  = runReaderT (unJConvT f) c

instance ReceiveMessages Channel JMsg where
  receiveMessages ch = jsonMessages ch

instance SendMessages Channel JMsg where
  sendMessage ch@(WebSocketCh{}) m
    = send_all ch buf
      where buf = UTF8B.fromString $ encode (jmsgToJson m)
  -- use null terminator if not websockets
  sendMessage ch m
    = send_all ch buf
      where buf = (UTF8B.fromString $ encode (jmsgToJson m)) `B.snoc` 0

mkSerial :: (Integral a) => a -> Serial
mkSerial v = fromJust . DB.fromVariant . DB.toVariant $ (fromIntegral v :: Word32)

class Signed a where
  signature :: a -> Maybe TL.Text

instance Signed JReq where signature = jreqSignature
instance Signed JSignal where signature = jsigSignature
instance Signed JResp where signature = jrespSignature
instance Signed JRespErr where signature = jerrSignature
instance Signed JMsg where
  signature (JMsgReq x) = signature x
  signature (JMsgSignal x) = signature x
  signature (JMsgResp x) = signature x
  signature (JMsgRespErr x) = signature x


marshaled serial m =
  case marshalMessage BigEndian serial m of
    Left _    -> Nothing
    Right buf -> Just $ (B.concat $ BL.toChunks buf)

mkMethodCallMsg :: Serial -> MethodCall -> Maybe Msg
mkMethodCallMsg serial m
  = Msg <$> pure (ReceivedMethodCall serial Nothing m)
        <*> marshaled serial m

mkSignalMsg :: Serial -> Signal -> Maybe Msg
mkSignalMsg serial m
  = Msg <$> pure (ReceivedSignal serial Nothing m)
        <*> marshaled serial m

mkErrorMsg :: Serial -> Error -> Maybe Msg
mkErrorMsg serial m
  = Msg <$> pure (ReceivedError serial Nothing m)
        <*> marshaled serial m

mkMethodReturnMsg :: Serial -> MethodReturn -> Maybe Msg
mkMethodReturnMsg serial m
  = Msg <$> pure (ReceivedMethodReturn serial Nothing m)
        <*> marshaled serial m

-- | type conversion mode
convMode :: forall m. (Monad m) => JMsg -> JConvT m TypeConvMode
convMode msg
  = return . fromMaybe GuessTypes =<< (provided `or` introspected)
  where
    provided, introspected :: JConvT m (Maybe TypeConvMode)
    provided
      = do let sig = join $ mkSignature `fmap` signature msg
           return (SpecifyTypes . signatureTypes <$> sig)
    introspected
      = do c <- context
           maybe_sig <- lift $ findSig c msg
           return (SpecifyTypes `fmap` maybe_sig)
    p `or` q
      = p >>= cont where
        cont (Just m) = return $ Just m
        cont Nothing  = q

jmsgErr tag act =
  do x <- act
     when (isNothing x) $
       liftIO $ warn ("converting " ++ tag ++ " to dbus message FAILED")
     return x

instance MonadIO m => MsgConvert (JConvT m) JMsg Msg where
  msgconvert m@(JMsgReq x) = MaybeT . jmsgErr "request" $
    do types <- convMode m
       return $ do m' <- convToMethodCall types x
                   let serial = mkSerial (intReqID $ jreqId x)
                   mkMethodCallMsg serial m'
  msgconvert m@(JMsgSignal x) = MaybeT . jmsgErr "signal" $
    do types <- convMode m
       return $ do m' <- convToSignal types x
                   let serial = mkSerial (intReqID $ jsigId x)
                   mkSignalMsg serial m'
  msgconvert m@(JMsgResp x) = MaybeT . jmsgErr "response" $
    do types <- convMode m
       return $ do m' <- convToMethodReturn types x
                   let serial = mkSerial (intReqID $ jrespId x)
                   mkMethodReturnMsg serial m'
  msgconvert m@(JMsgRespErr x) = MaybeT . jmsgErr "error response" $
    do types <- convMode m
       return $ do m' <- convToError types x
                   let serial = mkSerial (intReqID $ jerrId x)
                   mkErrorMsg serial m'

instance MonadIO m => MsgConvert (JConvT m) Msg JMsg where
  msgconvert (Msg rm _)
    = MaybeT $ case rm of
        ReceivedMethodCall s _ x -> err "method-call" $ return $ JMsgReq <$> convFromMethodCall (s,x)
        ReceivedMethodReturn s _ x -> err "method-return" $ return $ JMsgResp <$> convFromMethodReturn (s,x)
        ReceivedError  s _ x -> err "error" $ return $ JMsgRespErr <$> convFromError (s,x)
        ReceivedSignal s _ x -> err "signal" $ return $ JMsgSignal <$> convFromSignal (s,x)
        _ -> return Nothing
    where
      err tag act =
        do x <- act
           when (isNothing x) $
             liftIO $ warn ("converting " ++ tag ++ " to json message FAILED")
           return x

-- | dbus signature lookup via introspection
introspect :: (MonadRpc e m) => BusName -> ObjectPath -> m I.Object
introspect service p
  = do r <- rpcCallOnce $ RpcCall (Rpc.mkBusName_ . DB.strBusName $ service)
                                  (Rpc.mkObjectPath_ . DB.strObjectPath $ p)
                                  (Rpc.mkInterfaceName_ . DB.strInterfaceName $ introspectable)
                                  "Introspect" []
       ret (conv $ r)
    where
      conv [xmlv] = do xml <- fromVariant xmlv
                       I.fromXML p xml
      conv _ = error "unexpected introspect response"
      ret Nothing = error $ "failed to introspect " ++ show service ++ " " ++ show p
      ret (Just v) = return v

introspectable :: InterfaceName
introspectable = fromString "org.freedesktop.DBus.Introspectable"

type OMap = M.Map (BusName,ObjectPath) I.Object

sigFinder :: forall ctx e m. (FreezeIOM ctx (Either e) m, MonadRpc e m) => m (BusName -> ObjectPath -> InterfaceName -> MemberName -> m (Maybe [Type]))
sigFinder
  = do cache <- liftIO $ newMVar M.empty
       return $ finder cache
  where
    sig :: I.Object -> InterfaceName -> MemberName -> Maybe [Type]
    sig (I.Object _ intfs _) i m
      = do i' <- find_i i intfs
           m' <- find_m m i'
           mapM paramSig (inparams m')
        where
          find_i n = find (\(I.Interface n' _ _ _) -> n == n')
          find_m n (I.Interface _ methods _ _) = find (\(I.Method n' _ _) -> n == n') methods
          inparams (I.Method _ p _) = p
          paramSig (I.Parameter _ ps) = shead (signatureTypes ps)
          shead (x:xs) = Just x
          shead _ = Nothing

    finder :: (MVar OMap) -> BusName -> ObjectPath -> InterfaceName -> MemberName -> m (Maybe [Type])
    finder cache service path intf meth =
      do obj <- intro (service,path)
         case obj of
           Nothing  -> return Nothing
           Just obj -> return $ sig obj intf meth
      where
        intro :: (BusName,ObjectPath) -> m (Maybe I.Object)
        intro key@(s,p) = do
          context <- rpcGetContext
          let continue (Left ex) = return Nothing
              continue (Right v) = return (Just v)
          x <- freeze $ \context -> do
            liftIO . modifyMVar cache $ \ch ->
              case M.lookup key ch of
                Just v  -> return (ch, Right v)
                Nothing -> do
                  r <- thaw context (introspect s p :: m I.Object)
                  case r of
                    Right v -> return (M.insert key v ch, Right v)
                    Left er -> return (ch               , Left er)
          continue x
