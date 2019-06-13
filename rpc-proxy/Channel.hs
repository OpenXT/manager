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

{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable,TupleSections #-}

module Channel where

import Control.Concurrent
import Control.Monad
import Control.Applicative
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.WebSocket as W

import Text.Printf
import System.Posix

import qualified Tools.Argo as AR
import Tools.Log
import Tools.Serial
import Tools.IfM

import Settings
import Types
import Domain

import Foreign (castPtr)
import Data.ByteString.Internal ( createAndTrim )
import Data.ByteString.Unsafe ( unsafeUseAsCStringLen )

data Channel
   = StdSocket !NS.Socket Closed
   | ArgoSocket !Fd Closed
   | FdChann   !Fd Closed
   | WebSocketCh !W.WebSocket Channel Closed

type Closed = MVar Bool

instance Show Channel where
    show (StdSocket s _) = show s
    show (ArgoSocket f _) = printf "<argo: %s>" (show f)
    show (FdChann fd _) = printf "<fd: %s>" (show fd)
    show (WebSocketCh _ ch _) = printf "<websocket: %s>" (show ch)

makeIncomingTransport :: IncomingChannel -> IO Channel
makeIncomingTransport (FromArgo port) =
    do sock <- AR.socket NS.Stream
       AR.bind sock (AR.Addr port invalidDomID) invalidDomID
       AR.listen sock 5
       closed <- newMVar False
       return $ ArgoSocket sock closed
makeIncomingTransport (FromTCP port) =
    do sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
       NS.bindSocket sock (NS.SockAddrInet (fromIntegral port) NS.iNADDR_ANY)
       NS.listen sock 5
       closed <- newMVar False
       return $ StdSocket sock closed
makeIncomingTransport (FromUnixSocket path) =
    do sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
       NS.bindSocket sock (NS.SockAddrUnix path)
       NS.listen sock 5
       closed <- newMVar False
       return $ StdSocket sock closed
makeIncomingTransport (FromSerial path) =
    do f <- openSerial path
       closed <- newMVar False
       return $ FdChann f closed

wrapInWebSocket :: Channel -> IO Channel
wrapInWebSocket ch
  = WebSocketCh <$> W.create (recv ch) (send ch)
                <*> pure ch
                <*> newMVar False

makeOutgoingTransport :: OutgoingChannel -> IO (Channel,DomID)
makeOutgoingTransport (ToArgo port dom) =
    do sock <- AR.socket NS.Stream
       domid <- case dom of ByID   id -> return id
                            ByUuid u  -> resolv u =<< domidOfUuid u
       AR.connect sock (AR.Addr port (fromIntegral domid))
       closed <- newMVar False
       return (ArgoSocket sock closed, domid)
    where
      resolv _ (Just domid) = return domid
      resolv uuid Nothing = E.throw (NoUuid uuid)
makeOutgoingTransport (ToUnixSocket path) =
    do sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
       NS.connect sock (NS.SockAddrUnix path)
       closed <- newMVar False
       return (StdSocket sock closed, currentDomain)
makeOutgoingTransport (ToSerial path) =
    (,currentDomain) <$> (FdChann <$> openSerial path <*> newMVar False)

accept :: Channel -> IO (Channel, DomID)
accept (ArgoSocket s _) =
    do (s',addr) <- AR.accept s
       info $ printf "incoming connection from domain %d, port 0x%0x, fd is %s" (AR.addrDomID addr) (AR.addrPort addr) (show s')
       closed <- newMVar False
       return (ArgoSocket s' closed, fromIntegral $ AR.addrDomID addr)
accept (StdSocket s _) =
    do (s',addr) <- NS.accept s
       info $ printf "incoming connection from socket %s" (show s')
       closed <- newMVar False
       return (StdSocket s' closed, currentDomain)
accept ch@(FdChann fd _) = return (ch,-1)
accept (WebSocketCh _ ch _) = error "websockets accept: unsupported"

handshake :: Channel -> IO ()
handshake (WebSocketCh s _ _) = W.handshake s
handshake _ = return ()

recv :: Channel -> Int -> IO ByteString
recv x@(ArgoSocket fd _) sz = AR.recv fd sz 0
recv x@(StdSocket s  _) sz = NSB.recv s sz
recv x@(FdChann fd _) sz =
    do threadWaitRead fd
       createAndTrim sz $ \ptr -> fromIntegral <$> fdReadBuf fd ptr (fromIntegral sz)
recv (WebSocketCh s _ _) sz =
  frame <$> W.recvFrame s sz
  where
    frame Nothing = B.empty
    frame (Just (t,d)) = B.concat ( BL.toChunks d )

send :: Channel -> ByteString -> IO Int
send (ArgoSocket fd _) buf = AR.send fd buf 0
send (StdSocket s  _) buf = NSB.send s buf
send x@(FdChann fd _) buf = liftM fromIntegral .
    unsafeUseAsCStringLen buf $ \(ptr,sz) ->
        do threadWaitWrite fd
           fromIntegral <$> fdWriteBuf fd (castPtr ptr) (fromIntegral sz)

send (WebSocketCh s _ _) buf = W.sendFrame s (W.Text, BL.fromChunks [buf]) >> return (B.length buf)

send_all :: Channel -> ByteString -> IO ()
send_all s buf =
    send s buf >>= \sent ->
       when (sent < B.length buf) $
            do warn $ printf "short sent over %s -> %d out of %d" (show s) sent (B.length buf)
               send_all s (B.drop sent buf)

shutdownSend :: Channel -> IO ()
shutdownSend (StdSocket s closed) = NS.shutdown s NS.ShutdownSend
shutdownSend _ = return ()

shutdownRecv :: Channel -> IO ()
shutdownRecv (StdSocket s closed) = NS.shutdown s NS.ShutdownReceive
shutdownRecv _ = return ()

close :: Channel -> IO ()
close (ArgoSocket s closed) = perhaps closed (AR.close s)
close (StdSocket s closed) = perhaps closed (NS.sClose s)
close (FdChann fd closed) = perhaps closed (closeFd fd)
close (WebSocketCh s ch closed) = perhaps closed $ {- E.finally (W.shutdown s) -} (close ch)

perhaps closed f = modifyMVar_ closed $ \c -> True <$ unless c f

data BouncerErr = EOF Channel
                | ShortSend Channel Int Int
                | AuthError Channel String
                | NoUuid Uuid
                  deriving ( Typeable )

instance E.Exception BouncerErr

instance Show BouncerErr where
    show (EOF s) = printf "%s: end of stream reached" (show s)
    show (ShortSend s recv sent) = printf "%s send short: received %d, sent %d" (show s) recv sent
    show (AuthError s m) = printf "%s failed to authenticate %s" (show s) m
    show (NoUuid uuid) = printf "no domain with uuid %s" (show uuid)

