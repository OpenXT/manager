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

{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Vm.ProductProperty
       (
         ProductProperty (..)
       , ProductPropertyType (..)
       , ProductPropertyValue
       , PPUniqueID (..)
       , ppUniqueID
       , ppEnvKey
       , vmPPList
       , vmPPUpdate
       , vmPPRm
       , vmPPGetValue
       , vmPPSetValue
       , vmHasAnyPP
       , strPPT
       , pptStr
       ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Word
import Data.Int
import Text.Printf

import Rpc.Core
import Tools.Db

import Vm.Uuid

newtype PPUniqueID = PPUniqueID { strPPUniqueID :: String } deriving Eq

instance Show PPUniqueID where show = strPPUniqueID

data ProductProperty
   = ProductProperty
     { ppKey :: String
     , ppClass :: String
     , ppInstance :: String
     , ppType :: ProductPropertyType
     , ppDescription :: String
     , ppUserConfigurable :: Bool
     , ppPassword :: Bool
     , ppValue :: ProductPropertyValue
     }
 deriving (Eq,Show)

data ProductPropertyType
   = PPT_Uint8 | PPT_Sint8 | PPT_Uint16 | PPT_Sint16 | PPT_Uint32 | PPT_Sint32 | PPT_Uint64 | PPT_Sint64
   | PPT_String | PPT_Bool | PPT_Real32 | PPT_Real64
 deriving (Eq,Ord,Show)

type ProductPropertyValue = String

pptStrMapping = Map.fromList [
    (PPT_Uint8, "uint8")
  , (PPT_Sint8, "sint8")
  , (PPT_Uint16, "uint16")
  , (PPT_Sint16, "sint16")
  , (PPT_Uint32, "uint32")
  , (PPT_Sint32, "sint32")
  , (PPT_Uint64, "uint64")
  , (PPT_Sint64, "sint64")
  , (PPT_Real32, "real32")
  , (PPT_Real64, "real64")
  , (PPT_Bool, "Boolean")
  , (PPT_String, "String")
  ]

strPPTMapping
  = Map.fromList . map swap . Map.toList $ pptStrMapping where
    swap (k,v) = (v,k)

strPPT :: ProductPropertyType -> String
strPPT t = str where Just str = Map.lookup t pptStrMapping

pptStr :: String -> Maybe ProductPropertyType
pptStr s = Map.lookup s strPPTMapping

instance Marshall ProductPropertyType where
  dbWrite p v = dbWrite p (strPPT v)
  dbRead  p   = do
    str <- dbRead p
    return $ fromMaybe (error $ "invalid ProductPropertyType: " ++ show str) (pptStr str)

instance Marshall ProductProperty where
  dbWrite p pp = do
    w "key" (noempty . ppKey)
    w "class" (noempty . ppClass)
    w "instance" (noempty . ppInstance)
    w "type" (Just . strPPT . ppType)
    w "description" (noempty . ppDescription)
    w "userconfigurable" (Just . ppUserConfigurable)
    w "password" (Just . ppPassword)
    w "value" (noempty . ppValue)
    where
      w field f = dbMaybeWrite (p ++ "/" ++ field) (f pp)
      noempty str
        | null str  = Nothing
        | otherwise = Just str

  dbRead p =
    ProductProperty
      <$> r "key" ""
      <*> r "class" ""
      <*> r "instance" ""
      <*> r "type" PPT_String
      <*> r "description" ""
      <*> r "userconfigurable" False
      <*> r "password" False
      <*> r "value" ""
      where
        r k def = dbReadWithDefault def (p ++ "/" ++ k)

ppUniqueID :: ProductProperty -> PPUniqueID
ppUniqueID pp = PPUniqueID $
     mapNoEmpty (++ ".") (ppClass pp)
  ++ ppKey pp
  ++ mapNoEmpty ("." ++) (ppInstance pp)
  where
    mapNoEmpty f "" = ""
    mapNoEmpty f  s = f s

ppEnvKey = strPPUniqueID
ppGroupDBPath vm = vmPath vm ++ "/productproperty"
ppDBPath vm_id pp_id = ppGroupDBPath vm_id ++ "/" ++ (strPPUniqueID pp_id)

vmPath uuid = "/vm/" ++ show uuid

vmHasAnyPP :: MonadRpc e m => Uuid -> m Bool
vmHasAnyPP vm = ((> 0) . length) <$> dbList (ppGroupDBPath vm)

vmPPList :: MonadRpc e m => Uuid -> m [ProductProperty]
vmPPList vm_id = filter valid <$> dbRead (ppGroupDBPath vm_id) where
  valid = not . null . ppKey

vmPPUpdate :: MonadRpc e m => Uuid -> ProductProperty -> m ()
vmPPUpdate vm_id pp = dbWrite (ppDBPath vm_id (ppUniqueID pp)) pp

vmPPRm :: MonadRpc e m => Uuid -> PPUniqueID -> m ()
vmPPRm vm_id pp_id = dbRm (ppDBPath vm_id pp_id)

vmPPGetValue :: MonadRpc e m => Uuid -> PPUniqueID -> m String
vmPPGetValue vm_id pp_id = dbRead (ppDBPath vm_id pp_id ++ "/value")

vmPPSetValue :: MonadRpc e m => Uuid -> PPUniqueID -> String -> m ()
vmPPSetValue vm_id pp_id str = do
  pp <- dbRead (ppDBPath vm_id pp_id)
  case sanitiseValue str (ppType pp) of
    Nothing  -> error $ printf "value '%s' is invalid for product property '%s' of type %s" str (show $ ppUniqueID pp) (show $ ppType pp)
    Just str -> dbWrite (ppDBPath vm_id pp_id ++ "/value") str

sanitiseValue :: String -> ProductPropertyType -> Maybe String
sanitiseValue v = f where
  f PPT_String = Just v
  f PPT_Uint8 = bounded (minBound :: Word8) (maxBound :: Word8) =<< integer
  f PPT_Sint8 = bounded (minBound :: Int8) (maxBound :: Int8) =<< integer
  f PPT_Uint16 = bounded (minBound :: Word16) (maxBound :: Word16) =<< integer
  f PPT_Sint16 = bounded (minBound :: Int16) (maxBound :: Int16) =<< integer
  f PPT_Uint32 = bounded (minBound :: Word32) (maxBound :: Word32) =<< integer
  f PPT_Sint32 = bounded (minBound :: Int32) (maxBound :: Int32) =<< integer
  f PPT_Uint64 = bounded (minBound :: Word64) (maxBound :: Word64) =<< integer
  f PPT_Sint64 = bounded (minBound :: Int64) (maxBound :: Int64) =<< integer

  f PPT_Bool
    | v `elem` ["true", "false"] = Just v
    | otherwise = Nothing

  f PPT_Real32
    | parses (undefined :: Float) v = Just v
    | otherwise = Nothing
  f PPT_Real64
    | parses (undefined :: Double) v = Just v
    | otherwise = Nothing

  bounded min max x
    | x >= fromIntegral min, x <= fromIntegral max = Just v
    | otherwise = Nothing

  integer :: Maybe Integer
  integer = parse v

  parses :: forall a. Read a => a -> String -> Bool
  parses _ str = isJust (parse str :: Maybe a)

  parse :: Read a => String -> Maybe a
  parse v = case reads v of
    ((i,_):_) -> Just i
    _ -> Nothing

