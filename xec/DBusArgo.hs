--
-- Copyright (c) 2013 Citrix Systems, Inc.
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

{-# LANGUAGE ScopedTypeVariables, CPP #-}

module DBusArgo (
    domainSystemBus
  , remoteDomainBus
  ) where

import qualified Control.Exception as E
import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Data.Set as Set
import Data.Maybe
import Data.String
import Network.Socket (SocketType (..))
import System.Posix.Types
import System.Posix.IO

import qualified Data.ByteString as B
import qualified Network.DBus as D
import qualified Network.DBus.Actions as D

import qualified Tools.Argo as A

domainSystemBus :: Int -> IO D.DBusContext
remoteDomainBus :: Int -> Int -> IO D.DBusContext

domainSystemBus domain = remoteDomainBus domain 5555
remoteDomainBus domain argoPort = do
  let addr = A.Addr argoPort domain
  fd <- connect addr
  D.contextNewWith (argoTransport fd)
  where
    connect addr = A.socket Stream >>= \f ->
      -- be careful to close fd on connect error..
                  ( do setFdOption f NonBlockingRead False
                       A.connect f addr
                       setFdOption f NonBlockingRead True
                       return f )
                  `E.catch` connect_error f
                      where connect_error f (err::E.SomeException) = A.close f >> E.throw err
    argoTransport fd
      = D.DBusTransport { D.transportPut  = send fd
                        , D.transportGet  = recv fd
                        , D.transportClose = close fd }
    send  fd buf       = do sent <- A.send fd buf 0
                            if sent < (B.length buf)
                               then send fd (B.drop sent buf)
                               else return ()
      -- seems to be needed because stupid dbus bindings do recv 0 and argo blocks on that ?
    recv  fd 0         = return B.empty
    recv  fd sz        = recv_aux fd (fromIntegral sz)
    recv_aux fd sz     = do chunk <- A.recv fd (fromIntegral $ sz) 0
                            case B.length chunk of
                                0           -> return chunk
                                l | l >= sz -> return chunk
                                _           -> B.append chunk <$> recv_aux fd (sz - B.length chunk)

    close fd        = A.close fd
