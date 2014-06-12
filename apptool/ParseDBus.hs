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

{-# LANGUAGE NoMonomorphismRestriction #-}
module ParseDBus ( parseDBus ) where

import Control.Applicative (empty, (<$), (<$>) )
import qualified Data.ByteString.UTF8 as UTF8
import Text.ParserCombinators.Parsec
import Network.DBus

parseDBus :: String -> SignatureElem -> Maybe DBusValue
parseDBus x t = either (const Nothing) Just $ parse (dbusvalue_p True t) "" x

int_p :: (Read a, Integral a) => CharParser st a
int_p = read <$> do
  prefix <- option "" (string "-")
  (prefix++) <$> many1 digit

floating_p :: CharParser st Double
floating_p = read <$> do
  prefix <- option "" (string "-")
  a <- many1 digit
  b <- optionMaybe (char '.' >> many1 digit)
  return (prefix ++ a ++ case b of
             Nothing -> ""
             Just  b -> '.':b)

str_p :: Bool -> CharParser st String
str_p toplevel@True = getInput
str_p False = quoted '"' <|> quoted '\''
  where
    quoted q = do
      char q
      s <- escapedCh `manyTill` char q
      return s
      where
        escapedCh =
              ( '\\' <$ try (char '\\' >> char '\\') )
          <|> ( q    <$ try (char '\\' >> char q ) )
          <|> anyChar

dbusvalue_p :: Bool -> SignatureElem -> CharParser st DBusValue
dbusvalue_p toplevel t = go t where
  go SigByte = DBusByte <$> int_p
  go SigBool = DBusBoolean <$> bool_p
  go SigInt16 = DBusInt16 <$> int_p
  go SigUInt16 = DBusUInt16 <$> int_p
  go SigInt32 = DBusInt32 <$> int_p
  go SigUInt32 = DBusUInt32 <$> int_p
  go SigInt64 = DBusInt64 <$> int_p
  go SigUInt64 = DBusUInt64 <$> int_p
  go SigDouble = DBusDouble <$> floating_p
  go SigString = str_p toplevel >>= \x -> return $ DBusString (PackedString $ UTF8.fromString x)
  go SigObjectPath = str_p toplevel >>= \x -> return $ DBusObjectPath (ObjectPath x)
  go SigSignature = signature_p
  go (SigArray (SigDict kt vt)) = dict_p kt vt
  go (SigArray et) = array_p et
  go (SigDict kt vt) = dictentry_p kt vt
  go (SigStruct ets) = struct_p ets
  go SigUnixFD = DBusUnixFD <$> int_p
  go SigVariant = variant_p toplevel

bool_p :: CharParser st Bool
bool_p =  True  <$ ( string "true"  <|> string "1" )
      <|> False <$ ( string "false" <|> string "0" )

signature_p = empty

variant_p toplevel = empty

struct_p ts = do
  spaces
  char '['
  spaces
  r <- items ts
  spaces
  char ']'
  return (DBusStruct ts r)
  where
    items [] = return []
    items (t:ts) = do
      spaces
      x  <- dbusvalue_p False t
      xs <- case ts of
        [] -> return []
        _  -> spaces >> char ',' >> spaces >> items ts
      return (x:xs)

dict_p kt vt = do
  spaces
  char '{'
  spaces
  entries <- dictentry_p kt vt `sepBy` (try (spaces >> char ',' >> spaces))
  spaces
  char '}'
  spaces
  return $ DBusArray (SigDict kt vt) entries

dictentry_p kt vt = do
  spaces
  k <- dbusvalue_p False kt
  spaces
  char ':'
  spaces
  v <- dbusvalue_p False vt
  spaces
  return $ DBusDict k v

array_p t = do
  spaces
  char '['
  spaces
  items <- dbusvalue_p False t `sepBy` (try (spaces >> char ',' >> spaces))
  spaces
  char ']'
  spaces
  return $ DBusArray t items
