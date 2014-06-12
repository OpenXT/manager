--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

module JSONPretty where

import Text.JSON.Types
import Text.PrettyPrint.HughesPJ
import Data.Ratio
import Data.Char
import Numeric

pp_value         :: JSValue -> Doc
pp_value v        = case v of
    JSNull       -> pp_null
    JSBool x     -> pp_boolean x
    JSRational asf x -> pp_number asf x
    JSString x   -> pp_js_string x
    JSArray vs   -> pp_array vs
    JSObject xs  -> pp_js_object xs

pp_null          :: Doc
pp_null           = text "null"

pp_boolean       :: Bool -> Doc
pp_boolean True   = text "true"
pp_boolean False  = text "false"

pp_number        :: Bool -> Rational -> Doc
pp_number _ x | denominator x == 1 = integer (numerator x)
pp_number True x                   = float (fromRational x)
pp_number _    x                   = double (fromRational x)

pp_array         :: [JSValue] -> Doc
pp_array xs       = brackets $ fsep $ punctuate comma $ map pp_value xs

pp_string        :: String -> Doc
pp_string x       = doubleQuotes $ hcat $ map pp_char x
  where pp_char '\\'            = text "\\\\"
        pp_char '"'             = text "\\\""
        pp_char c | isControl c || fromEnum c >= 0x7f = uni_esc c
        pp_char c               = char c

        uni_esc c = text "\\u" <> text (pad 4 (showHex (fromEnum c) ""))

        pad n cs  | len < n   = replicate (n-len) '0' ++ cs
                  | otherwise = cs
          where len = length cs

braces' d = sep [text "{", nest 2 d, text "}"]

pp_object        :: [(String,JSValue)] -> Doc
pp_object xs      = braces' $ sep $ punctuate comma $ map pp_field xs
  where pp_field (k,v) = pp_string k <> colon <+> pp_value v

pp_js_string     :: JSString -> Doc
pp_js_string x    = pp_string (fromJSString x)

pp_js_object     :: JSObject JSValue -> Doc
pp_js_object x    = pp_object (fromJSObject x)
