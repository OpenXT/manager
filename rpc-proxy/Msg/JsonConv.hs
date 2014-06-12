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

module Msg.JsonConv
       ( TypeConvMode(..)
       , convToMethodCall
       , convToSignal
       , convToMethodReturn
       , convToError

       , convFromMethodCall
       , convFromMethodReturn
       , convFromSignal
       , convFromError
       ) where

import Control.Applicative
import Control.Monad
import Data.String
import Data.Int
import Data.Word
import Data.Maybe
import Data.Ratio
import qualified Data.Set
import qualified Data.Text.Lazy as TL

import DBus.Types
import DBus.Message

import Msg.JsonRpc

-- hacky crap due to bindings limitations
mkSerial :: (Integral a) => a -> Serial
mkSerial v = fromJust . fromVariant . toVariant $ (fromIntegral v :: Word32)
fromSerial :: (Integral a) => Serial -> a
fromSerial s = fromIntegral (read (show s) :: Word32)

data TypeConvMode
   = SpecifyTypes [Type]
   | GuessTypes

dtypes :: TypeConvMode -> [JArg] -> [Type]
dtypes (SpecifyTypes ts) _ = ts
dtypes GuessTypes as = map dtype as
  where
    dtype (JArgBool _) = DBusBoolean
    dtype (JArgString _) = DBusString
    dtype (JArgNumber _) = DBusWord32
    dtype (JArgArray _) = DBusArray DBusByte
    dtype (JArgDict _) = DBusDictionary DBusString DBusString

-- | JSON -> DBUS

convToMethodCall :: TypeConvMode -> JReq -> Maybe MethodCall
convToMethodCall sig r
  = MethodCall
      <$> mkObjectPath (jreqPath r)
      <*> mkMemberName (jreqMethod r)
      <*> pure (mkInterfaceName $ jreqInterface r)
      <*> pure (mkBusName $ jreqDest r)
      <*> pure Data.Set.empty
      <*> convJArgs sig (jreqArgs r)

convToSignal :: TypeConvMode -> JSignal -> Maybe Signal
convToSignal sig s
  = Signal
      <$> mkObjectPath (jsigPath s)
      <*> mkMemberName (jsigMethod s)
      <*> mkInterfaceName (jsigInterface s)
      <*> pure Nothing
      <*> convJArgs sig (jsigArgs s)

convToMethodReturn :: TypeConvMode -> JResp -> Maybe MethodReturn
convToMethodReturn sig r
  = MethodReturn
         <$> pure (mkSerial (intReqID $ jrespFor r))
         <*> pure Nothing
         <*> convJArgs sig (jrespArgs r)

convToError :: TypeConvMode -> JRespErr -> Maybe Error
convToError sig e
  = Error
         <$> mkErrorName (jerrName e)
         <*> pure (mkSerial (intReqID $ jerrFor e))
         <*> pure Nothing
         <*> convJArgs sig (jerrArgs e)

convJArgs :: TypeConvMode -> [JArg] -> Maybe [Variant]
convJArgs m as = mapM (uncurry convJArg) (zip (dtypes m as) as)

convJArg :: DBus.Types.Type -> JArg -> Maybe Variant
convJArg DBusString (JArgString x) = Just $ toVariant x
convJArg DBusBoolean (JArgBool x) = Just $ toVariant x
convJArg DBusInt16 (JArgNumber x) = Just $ toVariant (floor x :: Int16)
convJArg DBusInt32 (JArgNumber x) = Just $ toVariant (floor x :: Int32)
convJArg DBusInt64 (JArgNumber x) = Just $ toVariant (floor x :: Int64)
convJArg DBusByte (JArgNumber x) = Just $ toVariant (floor x :: Word8)
convJArg DBusWord16 (JArgNumber x) = Just $ toVariant (floor x :: Word16)
convJArg DBusWord32 (JArgNumber x) = Just $ toVariant (floor x :: Word32)
convJArg DBusWord64 (JArgNumber x) = Just $ toVariant (floor x :: Word64)
convJArg DBusDouble (JArgNumber x) = Just $ toVariant (realToFrac x :: Double)
convJArg DBusSignature (JArgString x) = toVariant <$> mkSignature x
convJArg DBusObjectPath (JArgString x) = toVariant <$> mkObjectPath x
convJArg (DBusArray et) (JArgArray xs)
  = liftM toVariant . arrayFromItems et =<< mapM (convJArg et) xs
convJArg (DBusDictionary kt vt) (JArgDict d)
  = fmap toVariant . dictionaryFromItems kt vt =<< mapM item d
  where item (k,v) = (,) <$> convJArg kt (JArgString k) <*> convJArg vt v
convJArg DBusVariant (JArgString x) = Just . toVariant $ toVariant x
convJArg DBusVariant (JArgBool x) = Just . toVariant $ toVariant x
convJArg DBusVariant (JArgNumber x) = Just . toVariant $ toVariant (floor x :: Int32)
-- ^^ FIXME: other more complex variant are going to fail
-- DBusStructure not supported
convJArg _ _ = Nothing

-- | DBUS -> JSON

convFromMethodCall :: (Serial, MethodCall) -> Maybe JReq
convFromMethodCall (serial, m)
  = JReq     (JReqID (fromSerial serial))
             (strMemberName $ methodCallMember m)
         <$> (strBusName <$> methodCallDestination m)
         <*> pure (strObjectPath $ methodCallPath m)
         <*> (strInterfaceName <$> methodCallInterface m)
         <*> pure (Just . TL.concat . map (typeCode . variantType) $ methodCallBody m)
         <*> mapM convVariant (methodCallBody m)

convFromSignal :: (Serial,Signal) -> Maybe JSignal
convFromSignal (serial,s)
  = JSignal
      (JReqID (fromSerial serial))
      (strObjectPath $ signalPath s)
      (strMemberName $ signalMember s)
      (strInterfaceName $ signalInterface s)
      (Just . TL.concat . map (typeCode . variantType) $ signalBody s)
      <$> mapM convVariant (signalBody s)

convFromMethodReturn :: (Serial,MethodReturn) -> Maybe JResp
convFromMethodReturn (serial,r) =
  JResp
    (JReqID (fromSerial serial))
    (JReqID (fromSerial $ methodReturnSerial r))
    (Just . TL.concat . map (typeCode . variantType) $ methodReturnBody r)
    <$> mapM convVariant (methodReturnBody r)

convFromError :: (Serial,Error) -> Maybe JRespErr
convFromError (serial,e) =
  JRespErr
       (JReqID $ fromSerial serial)
       (JReqID . fromSerial $ errorSerial e)
       (strErrorName $ errorName e)
       (Just . TL.concat . map (typeCode . variantType) $ errorBody e)
       <$> mapM convVariant (errorBody e)

convVariant :: Variant -> Maybe JArg
convVariant v = go (variantType v) where
  go DBusBoolean = JArgBool <$> fromVariant v
  go DBusByte = JArgNumber . toRational <$> (fromVariant v :: Maybe Word8)
  go DBusInt16 = JArgNumber . toRational <$> (fromVariant v :: Maybe Int16)
  go DBusInt32 = JArgNumber . toRational <$> (fromVariant v :: Maybe Int32)
  go DBusInt64 = JArgNumber . toRational <$> (fromVariant v :: Maybe Int64)
  go DBusWord16 = JArgNumber . toRational <$> (fromVariant v :: Maybe Word16)
  go DBusWord32 = JArgNumber . toRational <$> (fromVariant v :: Maybe Word32)
  go DBusWord64 = JArgNumber . toRational <$> (fromVariant v :: Maybe Word64)
  go DBusDouble = JArgNumber . toRational <$> (fromVariant v :: Maybe Double)
  go DBusString = JArgString <$> fromVariant v
  go DBusSignature = JArgString . strSignature <$> fromVariant v
  go DBusObjectPath = JArgString . strObjectPath <$> fromVariant v
  go DBusVariant = convVariant =<< fromVariant v
  go (DBusStructure ts) =
    do Structure items <- fromVariant v
       JArgArray <$> mapM convVariant items
  go (DBusArray et)
    = liftM JArgArray . mapM convVariant . arrayItems =<< fromVariant v
  go (DBusDictionary _ _)
    = let item (k, dv) = (,) <$> (fromVariant k :: Maybe TL.Text) <*> convVariant dv in
      liftM JArgDict . mapM item . dictionaryItems =<< fromVariant v
