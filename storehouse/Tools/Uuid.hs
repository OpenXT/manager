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

{-# LANGUAGE EmptyDataDecls #-}
{- Uuid adapted from the Hit project's ref module: Data.Git.Ref
 -}
module Tools.Uuid
    ( Uuid
    , uuidGen
    , uuidFromBinary
    , uuidFromString
    , uuidFromStringStrip
    , uuidFromDBus
    , uuidToDBus
    , uuidStrUnderscore
    , uuidNull
    , KindVDI
    , KindSR
    , KindVHD
    ) where

import Data.Maybe
import Data.String
import Data.List
import qualified Data.Text as T
import Data.Bits
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (unsafeCreate)
import qualified Data.ByteString.Unsafe as B (unsafeIndex)
import qualified Data.ByteString.Char8 as BC

import Text.Printf
import Rpc.Core

import Control.Monad (forM_)
import Foreign.Storable

newtype Uuid a = Uuid B.ByteString deriving (Eq,Ord)

instance Show (Uuid a) where
	show (Uuid s) = intercalate "-" $ map disp [[0..3],[4,5],[6,7],[8,9],[10..15]]
		where disp = concatMap (printf "%02x" . B.index s)

instance IsString (Uuid a) where
	fromString str = uuidFromString str

uuidFromString :: String -> Uuid a
uuidFromString s
        | length s  /= 36 = error ("not a valid uuid: " ++ show s)
        | length s' /= 32 = error ("not a valid uuid: " ++ show s)
        | otherwise       = Uuid $ B.unsafeCreate 16 populateRef
		where
			s' = filter (/= '-') s

			populateRef ptr = forM_ [0..15] $ \i -> do
				let v = (unhex (s' !! (i*2+0)) `shiftL` 4) .|. unhex (s' !! (i*2+1))
				pokeElemOff ptr (i+0) v

			unhex :: Char -> Word8
			unhex '0' = 0
			unhex '1' = 1
			unhex '2' = 2
			unhex '3' = 3
			unhex '4' = 4
			unhex '5' = 5
			unhex '6' = 6
			unhex '7' = 7
			unhex '8' = 8
			unhex '9' = 9  -- '9'
			unhex 'A' = 10 -- 'A'
			unhex 'B' = 11
			unhex 'C' = 12
			unhex 'D' = 13
			unhex 'E' = 14
			unhex 'F' = 15 -- 'F'
			unhex 'a' = 10 -- 'a'
			unhex 'b' = 11
			unhex 'c' = 12
			unhex 'd' = 13
			unhex 'e' = 14
			unhex 'f' = 15 -- 'f'
			unhex _   = error "error fromHex: not a valid hex character"

--
-- Make Uuid instance of Variable for easier marshalling
--
instance Variable (Uuid a) where
	toVariant uuid = toVariant (show uuid)

	fromVariant v =
          uuidFromString `fmap` fromVariant v

uuidFromBinary :: B.ByteString -> Uuid a
uuidFromBinary b
    | B.length b == 16 = Uuid b
    | otherwise        = error "not a valid uuid"

uuidFromStringStrip :: String -> Uuid a
uuidFromStringStrip = fromString . strip where strip = T.unpack . T.strip . T.pack

uuidGen :: IO (Uuid a)
uuidGen = uuidFromStringStrip `fmap` readFile "/proc/sys/kernel/random/uuid"


uuidToDBus :: Uuid a -> String
uuidToDBus = map subst . show where subst c = if c == '-' then '_' else c

{-# DEPRECATED uuidStrUnderscore "move to uuidToDBus" #-}
uuidStrUnderscore :: Uuid a -> String
uuidStrUnderscore = uuidToDBus

uuidFromDBus :: String -> Uuid a
uuidFromDBus = uuidFromString . map subst where subst c = if c == '_' then '-' else c

uuidNull :: (Uuid a)
uuidNull = Uuid (B.replicate 16 0)

data KindVDI
data KindSR
data KindVHD
