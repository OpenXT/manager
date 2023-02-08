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

module Msg.JsonRpc
       ( JReq(..)
       , JReqID(..)
       , JResp(..)
       , JRespErr(..)
       , JSignal(..)
       , JArg(..)

       , jreqFromJson
       , jsignalFromJson
       , jrespFromJson
       , jerrFromJson

       , jreqToJson
       , jsignalToJson
       , jrespToJson
       , jerrToJson
       )
       where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Text.Lazy as TL
import Text.JSON

import Tools.Text

newtype JReqID = JReqID { intReqID :: Integer }

data JReq =
     JReq { jreqId :: JReqID
          , jreqMethod :: String
          , jreqDest :: String
          , jreqPath :: String
          , jreqInterface :: String
          , jreqSignature :: Maybe String             -- optional dbus sig for arguments to ease conversion
          , jreqArgs :: [JArg] }

data JSignal
   = JSignal { jsigId :: JReqID
             , jsigPath :: String
             , jsigMethod :: String
             , jsigInterface :: String
             , jsigSignature :: Maybe String             -- optional dbus sig for arguments to ease conversion
             , jsigArgs :: [JArg] }

data JResp
   = JResp { jrespId :: JReqID
           , jrespFor :: JReqID
           , jrespSignature :: Maybe String             -- optional dbus sig for arguments to ease conversion
           , jrespArgs :: [JArg] }

data JRespErr
   = JRespErr { jerrId :: JReqID
              , jerrFor :: JReqID
              , jerrName :: String
              , jerrSignature :: Maybe String             -- optional dbus sig for arguments to ease conversion
              , jerrArgs :: [JArg] }

data JArg =
     JArgBool Bool
   | JArgNumber Rational
   | JArgString String
   | JArgArray [JArg]
   | JArgDict [(String,JArg)]

sget k m
  = k `lookup` m >>= unp where unp (JSString s) = return . fromJSString $ s
                               unp _ = Nothing
nget k m
  = k `lookup` m >>= unp where unp (JSRational _ r) = Just $ floor r
                               unp _ = Nothing


jreqFromJson :: JSValue -> Maybe JReq
jreqFromJson (JSObject v)
  = do let kv = fromJSObject v
       req kv
  where
    req kv
      = JReq
          <$> (JReqID <$> ("id" `nget` kv))
          <*> s "method"
          <*> s "destination"
          <*> s "path"
          <*> s "interface"
          <*> pure (s "dbus-signature")
          <*> (mapM arg =<< arr "args")
            where s   k = k `sget` kv
                  arr k = k `lookup` kv >>= unp where unp (JSArray x) = Just x
                                                      unp _ = Nothing

jreqFromJson _
  = Nothing

jsignalFromJson :: JSValue -> Maybe JSignal
jsignalFromJson (JSObject v)
  = do let kv = fromJSObject v
       from kv
  where
    from kv
      = JSignal
          <$> (JReqID <$> ("id" `nget` kv))
          <*> s "path"
          <*> s "method"
          <*> s "interface"
          <*> pure (s "dbus-signature")
          <*> (mapM arg =<< arr "args")
            where s   k = k `sget` kv
                  arr k = k `lookup` kv >>= unp where unp (JSArray x) = Just x
                                                      unp _ = Nothing
jsignalFromJson _ = Nothing

jrespFromJson :: JSValue -> Maybe JResp
jrespFromJson (JSObject v) = from (fromJSObject v)
  where
    from kv
      = JResp
          <$> (JReqID <$> ("id" `nget` kv))
          <*> (JReqID <$> ("response-to" `nget` kv))
          <*> pure (s "dbus-signature")
          <*> (mapM arg =<< arr "args")
            where s   k = k `sget` kv
                  arr k = k `lookup` kv >>= unp where unp (JSArray x) = Just x
                                                      unp _ = Nothing
jrespFromJson _ = Nothing

jerrFromJson :: JSValue -> Maybe JRespErr
jerrFromJson (JSObject v) = from (fromJSObject v)
  where
    from kv
      = JRespErr
          <$> (JReqID <$> ("id" `nget` kv))
          <*> (JReqID <$> ("response-to" `nget` kv))
          <*> s "name"
          <*> pure (s "dbus-signature")
          <*> (mapM arg =<< arr "args")
            where s   k = k `sget` kv
                  arr k = k `lookup` kv >>= unp where unp (JSArray x) = Just x
                                                      unp _ = Nothing
jerrFromJson _ = Nothing

str = JSString . toJSString

jreqToJson :: JReq -> JSValue
jreqToJson r
  = JSObject $ toJSObject [
      ( "id", JSRational False (fromIntegral $ intReqID $ jreqId r))
    , ( "type", str "request" )
    , ( "method", str (jreqMethod r))
    , ( "destination", str (jreqDest r))
    , ( "path", str (jreqPath r))
    , ( "interface", str (jreqInterface r))
    , ( "args", JSArray (map value $ jreqArgs r))
    ]
    where str = JSString . toJSString

jsignalToJson :: JSignal -> JSValue
jsignalToJson s
  = JSObject $ toJSObject [
      ( "id", JSRational False (fromIntegral $ intReqID $ jsigId s))
    , ( "type", str "signal" )
    , ( "interface", str (jsigInterface s) )
    , ( "member", str (jsigMethod s) )
    , ( "path", str (jsigPath s) )
    , ( "args", JSArray (map value $ jsigArgs s) )
    ]
    where str = JSString . toJSString

jrespToJson :: JResp -> JSValue
jrespToJson r
  = JSObject $ toJSObject [
      ( "id", JSRational False (fromIntegral $ intReqID $ jrespId r))
    , ( "type", str "response" )
    , ( "response-to", str $ show $ intReqID (jrespFor r) )
    , ( "args", JSArray (map value $ jrespArgs r) )
    ]


jerrToJson :: JRespErr -> JSValue
jerrToJson e
  = JSObject $ toJSObject [
      ( "id", JSRational False (fromIntegral $ intReqID $ jerrId e))
    , ( "type", str "error" )
    , ( "response-to", str $ show $ intReqID (jerrFor e) )
    , ( "name", str (jerrName e) )
    , ( "args", JSArray (map value $ jerrArgs e) )
    ]

arg :: JSValue -> Maybe JArg
arg (JSBool v) = Just $ JArgBool v
arg (JSRational _ v) = Just $ JArgNumber v
arg (JSString v) = Just $ JArgString (fromJSString v)
arg (JSArray xs) = JArgArray <$> mapM arg xs
arg (JSObject o)
  = JArgDict <$> mapM f (fromJSObject o)
  where f (k,v) = (,) <$> pure k <*> arg v
arg _ = Nothing

value :: JArg -> JSValue
value (JArgBool v) = JSBool v
value (JArgNumber v) = JSRational False v
value (JArgString v) = JSString $ toJSString v
value (JArgArray xs) = JSArray (map value xs)
value (JArgDict kv) = JSObject $ toJSObject (map f kv) where f (k,v) = (k, value v)

