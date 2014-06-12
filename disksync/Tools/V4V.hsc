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

{-# LANGUAGE CPP,ForeignFunctionInterface #-}
module Tools.V4V ( Addr (..)
                 , DomID
                 , SocketType (..)
                 , socket, close, bind, connect, listen, accept, send, recv
                 ) where

import Data.Word
import qualified Data.ByteString as B
import Data.ByteString.Internal ( createAndTrim )
import Data.ByteString.Unsafe ( unsafeUseAsCStringLen )
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Exception
import Foreign
import Foreign.C.Types
import Foreign.C.Error
import System.Posix.Types
import Network.Socket ( SocketType, packSocketType )
import System.IO
import System.Posix.IO
import Tools.Log
import Data.Bits

type DomID = Int

data Addr = Addr { addrPort  :: !Int
                 , addrDomID :: !DomID } deriving Show

#include <libv4v.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Addr where
    alignment _ = #{alignment v4v_addr_t}
    sizeOf    _ = #{size v4v_addr_t}
    peek p      = do port  <- #{peek v4v_addr_t, port} p
                     domid <- ((.&.) 0xFFFF) <$> #{peek v4v_addr_t, domain} p
                     return $ Addr port domid
    poke p v    = do #{poke v4v_addr_t, port} p (addrPort v)
                     #{poke v4v_addr_t, domain} p (addrDomID v)

-- subset of libv4v.h
foreign import ccall "libv4v.h v4v_socket" c_v4v_socket    :: CInt -> IO CInt
foreign import ccall "libv4v.h v4v_close" c_v4v_close      :: CInt -> IO CInt
foreign import ccall "libv4v.h v4v_bind" c_v4v_bind        :: CInt -> Ptr Addr -> CInt -> IO CInt
foreign import ccall "libv4v.h v4v_connect" c_v4v_connect  :: CInt -> Ptr Addr -> IO CInt
foreign import ccall "libv4v.h v4v_listen" c_v4v_listen    :: CInt -> CInt -> IO CInt
foreign import ccall "libv4v.h v4v_accept" c_v4v_accept    :: CInt -> Ptr Addr -> IO CInt
foreign import ccall "libv4v.h v4v_send" c_v4v_send        :: CInt -> Ptr Word8 -> CUInt -> CInt -> IO CInt
foreign import ccall "libv4v.h v4v_recv" c_v4v_recv        :: CInt -> Ptr Word8 -> CUInt -> CInt -> IO CInt

int :: (Integral a, Num b) => a -> b
int = fromIntegral

socket :: SocketType -> IO Fd
socket t =
    do fd <- int <$> throwErrnoIfMinus1 "socket" ( c_v4v_socket (packSocketType t) )
       setFdOption fd NonBlockingRead True
       return fd

close :: Fd -> IO ()
close f = throwErrnoIfMinus1 "close" ( c_v4v_close (int f) ) >> return ()

bind :: Fd -> Addr -> DomID -> IO ()
bind f addr partner = do
    with addr $ \addr_p ->
        throwErrnoIfMinus1 "bind" $ c_v4v_bind (int f) addr_p (int partner)
    return ()

connect :: Fd -> Addr -> IO ()
connect f addr = do
    with addr $ \addr_p ->
        throwErrnoIfMinus1 "connect" $ c_v4v_connect (int f) addr_p
    return ()

listen :: Fd -> Int -> IO ()
listen f backlog = do
    throwErrnoIfMinus1 "listen" $ c_v4v_listen (int f) (int backlog)
    return ()

accept :: Fd -> IO (Fd, Addr)
accept f =
    alloca $ \addr_p ->
        do f' <- throwErrnoIfMinus1RetryMayBlock "accept" (c_v4v_accept (int f) addr_p) (threadWaitRead f)
           setFdOption (int f') NonBlockingRead True
           addr <- peek addr_p
           return (int f', addr)

send :: Fd -> B.ByteString -> Int -> IO Int
send f buf flags =
    fmap int $
         unsafeUseAsCStringLen buf $ \(ptr,sz) ->
             throwErrnoIfMinus1RetryMayBlock "send"
             ( c_v4v_send (int f) (castPtr ptr) (int sz) (int flags) )
             ( threadWaitWrite f )

recv :: Fd -> Int -> Int -> IO B.ByteString
recv f sz flags =
    createAndTrim sz $ \ptr ->
        fmap int $
             throwErrnoIfMinus1RetryMayBlock "recv"
             ( c_v4v_recv (int f) (castPtr ptr) (int sz) (int flags) )
             ( threadWaitRead f )
