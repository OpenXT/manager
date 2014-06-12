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

module OVF.Manifest
       ( Manifest
       , FileDigest (..)
       , ChecksumAlgorithm (..)
       , parseManifest
       )
       where

import Control.Applicative ( (<$>), (<*>), (<$), (<*) )
import Control.Monad
import Text.ParserCombinators.Parsec

data ChecksumAlgorithm = Sha1 | Sha256 deriving (Eq, Show)
data FileDigest = FileDigest FilePath ChecksumAlgorithm Integer
type Manifest = [FileDigest]

eol_p = (char '\r' >> char '\n') <|> char '\n'

digest_p
  = read . ("0x"++) <$> many1 hexDigit

algorithm_p
  =
      try (string "SHA1"   >> return Sha1)
  <|>     (string "SHA256" >> return Sha256)
  <?> "algorithm"      
  
filedigest_p = do
  algorithm <- algorithm_p
  char '('
  filename <- many1 (noneOf ")")
  string ")= "
  digest <- digest_p
  return $ FileDigest filename algorithm digest

manifest_p
  = skipMany eol_p >> sepEndBy filedigest_p (skipMany1 eol_p) <* eof
    <?> "file digests"

parseManifest :: String -> Either ParseError Manifest
parseManifest input = parse manifest_p "" input
