--
-- Copyright (c) 2017 Assured Information Security, Inc.
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

{-# LANGUAGE ForeignFunctionInterface #-}

-- description: Replace plaintext WPA-PSK passwords with their hashed forms.
-- date: 3/20/2017

module Migrations.M_35 (migration) where

import UpgradeEngine
import Data.ByteString.Internal (createAndTrim, create)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString.Char8 as B8
import Foreign
import Foreign.C
import System.IO.Unsafe
import Text.Printf
import Data.Maybe
import Control.Applicative

migration = Migration {
              sourceVersion = 35
            , targetVersion = 36
            , actions = act
            }

-- Calculate a PKCS5-PBKDF2 SHA1-HMAC suitable for password hashing.
-- This is a part of the HsOpenSSL library (under CC0 license).
pkcs5_pbkdf2_hmac_sha1 :: B8.ByteString -- ^ password
                       -> B8.ByteString -- ^ salt
                       -> Int           -- ^ iterations
                       -> Int           -- ^ destination key length
                       -> B8.ByteString -- ^ destination key
pkcs5_pbkdf2_hmac_sha1 pass salt iter dkeylen =
  unsafePerformIO $
  unsafeUseAsCStringLen pass $ \(passdata, passlen) ->
  unsafeUseAsCStringLen salt $ \(saltdata, saltlen) ->
  create dkeylen $ \dkeydata ->
      _PKCS5_PBKDF2_HMAC_SHA1
           passdata (fromIntegral passlen)
           saltdata (fromIntegral saltlen)
           (fromIntegral iter) (fromIntegral dkeylen) (castPtr dkeydata)
      >> return ()

foreign import ccall unsafe "PKCS5_PBKDF2_HMAC_SHA1" _PKCS5_PBKDF2_HMAC_SHA1 :: Ptr CChar -> CInt
                                                                             -> Ptr CChar -> CInt
                                                                             -> CInt -> CInt -> Ptr CChar
                                                                             -> IO CInt
-- End of part of the HsOpenSSL library

hex :: B8.ByteString -> String
hex = concatMap (printf "%02x") . B8.unpack

--Convert any plaintext PSK passwords to their hashed form if the given connection is a WPA-PSK one.
checkPlaintextPSKPasswords :: JSValue -> JSValue
checkPlaintextPSKPasswords connectionInfo
  | isPSK connectionInfo && isPlaintext connectionInfo = hashPlaintextPassword connectionInfo
  | otherwise = connectionInfo

--Convert the password of the given connection to its hashed form.
hashPlaintextPassword :: JSValue -> JSValue
hashPlaintextPassword connectionInfo = jsSet "802-11-wireless-security/psk" (jsBoxString $ hashPassword password ssid) connectionInfo
  where password = fromMaybe "" $ jsUnboxString <$> jsGet "802-11-wireless-security/psk" connectionInfo
        ssid = fromMaybe "" $ jsUnboxString <$> jsGet "802-11-wireless/ssid" connectionInfo

--Calculate the hashed form of a WPA-PSK password given a password and SSID.
hashPassword :: String -> String -> String
hashPassword password ssid = hex (pkcs5_pbkdf2_hmac_sha1 (B8.pack password) (B8.pack ssid) 4096 32)

isPSK :: JSValue -> Bool
isPSK connectionInfo = fromMaybe (jsBoxString "") (jsGet "802-11-wireless-security/key-mgmt" connectionInfo) == jsBoxString "wpa-psk"

isPlaintext :: JSValue -> Bool
isPlaintext connectionInfo = length (jsUnboxString (fromMaybe (jsBoxString "") (jsGet "802-11-wireless-security/psk" connectionInfo))) < 64

act :: IO ()
act = xformDomStoreJSON $ jsMapChildren checkPlaintextPSKPasswords "/nm-connections"
