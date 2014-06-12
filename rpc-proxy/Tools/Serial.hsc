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

{-# LANGUAGE CPP,ForeignFunctionInterface,ScopedTypeVariables #-}
module Tools.Serial ( openSerial ) where

import System.Posix
import Foreign
import Foreign.C.String
import Foreign.C.Types

foreign import ccall unsafe "open_serial_raw"
        c_open_serial_raw :: CString -> IO CInt

openSerial :: FilePath -> IO Fd
openSerial path =
    do fd <- withCString path $ \pathS -> c_open_serial_raw pathS
       if ( fd < 0 )
          then error ("failed to open " ++ show path)
          else return (fromIntegral fd)
