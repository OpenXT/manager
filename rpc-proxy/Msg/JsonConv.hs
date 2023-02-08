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

import DBus.Internal.Types
import DBus.Internal.Message

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
    dtype (JArgBool _) = TypeBoolean
    dtype (JArgString _) = TypeString
    dtype (JArgNumber _) = TypeWord32
    dtype (JArgArray _) = TypeArray TypeWord8
    dtype (JArgDict _) = TypeDictionary TypeString TypeString

-- | JSON -> DBUS

convToMethodCall :: TypeConvMode -> JReq -> Maybe MethodCall
convToMethodCall sig r
  = MethodCall
      <$> parseObjectPath (jreqPath r)
      <*> parseMemberName (jreqMethod r)
      <*> pure (parseInterfaceName $ jreqInterface r)
      <*> pure (parseBusName $ jreqDest r)
      <*> pure Data.Set.empty
      <*> convJArgs sig (jreqArgs r)

convToSignal :: TypeConvMode -> JSignal -> Maybe Signal
convToSignal sig s
  = Signal
      <$> parseObjectPath (jsigPath s)
      <*> parseMemberName (jsigMethod s)
      <*> parseInterfaceName (jsigInterface s)
      <*> pure Nothing
      <*> convJArgs sig (jsigArgs s)

convToMethodReturn :: TypeConvMode -> JResp -> Maybe MethodReturn
convToMethodReturn sig r
  = MethodReturn
         <$> pure (mkSerial (intReqID $ jrespFor r))
         <*> pure Nothing
         <*> convJArgs sig (jrespArgs r)

convToError :: TypeConvMode -> JRespErr -> Maybe MethodError
convToError sig e
  = MethodError
         <$> parseErrorName (jerrName e)
         <*> pure (mkSerial (intReqID $ jerrFor e))
         <*> pure Nothing
         <*> convJArgs sig (jerrArgs e)

convJArgs :: TypeConvMode -> [JArg] -> Maybe [Variant]
convJArgs m as = mapM (uncurry convJArg) (zip (dtypes m as) as)

convJArg :: DBus.Internal.Types.Type -> JArg -> Maybe Variant
convJArg TypeString (JArgString x) = Just $ toVariant x
convJArg TypeBoolean (JArgBool x) = Just $ toVariant x
convJArg TypeInt16 (JArgNumber x) = Just $ toVariant (floor x :: Int16)
convJArg TypeInt32 (JArgNumber x) = Just $ toVariant (floor x :: Int32)
convJArg TypeInt64 (JArgNumber x) = Just $ toVariant (floor x :: Int64)
convJArg TypeWord8 (JArgNumber x) = Just $ toVariant (floor x :: Word8)
convJArg TypeWord16 (JArgNumber x) = Just $ toVariant (floor x :: Word16)
convJArg TypeWord32 (JArgNumber x) = Just $ toVariant (floor x :: Word32)
convJArg TypeWord64 (JArgNumber x) = Just $ toVariant (floor x :: Word64)
convJArg TypeDouble (JArgNumber x) = Just $ toVariant (realToFrac x :: Double)
convJArg TypeSignature (JArgString x) = toVariant <$> parseSignature $ x
convJArg TypeObjectPath (JArgString x) = toVariant <$> parseObjectPath $ x
convJArg (TypeArray et) (JArgArray xs)
  = liftM toVariant . arrayFromItems et =<< mapM (convJArg et) xs
convJArg (TypeDictionary kt vt) (JArgDict d)
  = fmap toVariant . dictionaryFromItems kt vt =<< mapM item d
  where item (k,v) = (,) <$> convJArg kt (JArgString k) <*> convJArg vt v
convJArg TypeVariant (JArgString x) = Just . toVariant $ toVariant x
convJArg TypeVariant (JArgBool x) = Just . toVariant $ toVariant x
convJArg TypeVariant (JArgNumber x) = Just . toVariant $ toVariant (floor x :: Int32)
-- ^^ FIXME: other more complex variant are going to fail
-- TypeStructure not supported
convJArg _ _ = Nothing

-- | DBUS -> JSON

convFromMethodCall :: (Serial, MethodCall) -> Maybe JReq
convFromMethodCall (serial, m)
  = JReq     (JReqID (fromSerial serial))
             (formatMemberName $ methodCallMember m)
         <$> (formatBusName <$> methodCallDestination m)
         <*> pure (formatObjectPath $ methodCallPath m)
         <*> (formatInterfaceName <$> methodCallInterface m)
         <*> pure (Just . concat . map (typeCode . variantType) $ methodCallBody m)
         <*> mapM convVariant (methodCallBody m)

convFromSignal :: (Serial,Signal) -> Maybe JSignal
convFromSignal (serial,s)
  = JSignal
      (JReqID (fromSerial serial))
      (formatObjectPath $ signalPath s)
      (formatMemberName $ signalMember s)
      (formatInterfaceName $ signalInterface s)
      (Just . concat . map (typeCode . variantType) $ signalBody s)
      <$> mapM convVariant (signalBody s)

convFromMethodReturn :: (Serial,MethodReturn) -> Maybe JResp
convFromMethodReturn (serial,r) =
  JResp
    (JReqID (fromSerial serial))
    (JReqID (fromSerial $ methodReturnSerial r))
    (Just . concat . map (typeCode . variantType) $ methodReturnBody r)
    <$> mapM convVariant (methodReturnBody r)

convFromError :: (Serial,MethodError) -> Maybe JRespErr
convFromError (serial,e) =
  JRespErr
       (JReqID $ fromSerial serial)
       (JReqID . fromSerial $ methodErrorSerial e)
       (formatErrorName $ methodErrorName e)
       (Just . concat . map (typeCode . variantType) $ methodErrorBody e)
       <$> mapM convVariant (methodErrorBody e)

convVariant :: Variant -> Maybe JArg
convVariant v = go (variantType v) where
  go TypeBoolean = JArgBool <$> fromVariant v
  go TypeWord8 = JArgNumber . toRational <$> (fromVariant v :: Maybe Word8)
  go TypeInt16 = JArgNumber . toRational <$> (fromVariant v :: Maybe Int16)
  go TypeInt32 = JArgNumber . toRational <$> (fromVariant v :: Maybe Int32)
  go TypeInt64 = JArgNumber . toRational <$> (fromVariant v :: Maybe Int64)
  go TypeWord16 = JArgNumber . toRational <$> (fromVariant v :: Maybe Word16)
  go TypeWord32 = JArgNumber . toRational <$> (fromVariant v :: Maybe Word32)
  go TypeWord64 = JArgNumber . toRational <$> (fromVariant v :: Maybe Word64)
  go TypeDouble = JArgNumber . toRational <$> (fromVariant v :: Maybe Double)
  go TypeString = JArgString <$> fromVariant v
  go TypeSignature = JArgString . formatSignature <$> fromVariant v
  go TypeObjectPath = JArgString . formatObjectPath <$> fromVariant v
  go TypeVariant = convVariant =<< fromVariant v
  go (TypeStructure ts) =
    do Structure items <- fromVariant v
       JArgArray <$> mapM convVariant items
  go (TypeArray et)
    = liftM JArgArray . mapM convVariant . arrayItems =<< fromVariant v
  go (TypeDictionary _ _)
    = let item (k, dv) = (,) <$> (fromVariant k :: Maybe String) <*> convVariant dv in
      liftM JArgDict . mapM item . dictionaryItems =<< fromVariant v
