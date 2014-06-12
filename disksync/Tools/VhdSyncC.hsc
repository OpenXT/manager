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
module Tools.VhdSyncC (
			forkCloseAndExec
                      ) where

import IO
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

import System.Posix.Types

foreign import ccall unsafe "fork_close_and_exec"
        c_fork_close_and_exec :: CInt -> Ptr CInt -> Ptr CString -> IO CInt

withCStringArray :: [String] -> ([CString] -> IO a) -> IO a
withCStringArray s f =
    bracket (allocStrs s) freeStrs f
    where
      allocStrs = mapM newCString
      freeStrs = mapM_ free

forkCloseAndExec :: [Int] -> [String] -> IO ProcessID
forkCloseAndExec leaveOpenFds argv =
    withArrayLen openfds $ \num_fds fds_ptr ->
    withCStringArray argv $ \argv_arr ->
    withArray0 nullPtr argv_arr $ \argv_ptr ->
        do pid <- c_fork_close_and_exec (fromIntegral num_fds) fds_ptr argv_ptr
           return $ fromIntegral pid
    where
      openfds = map fromIntegral leaveOpenFds
