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

module Msg.DBus
       ( Msg (..)
       , unmarshaledMessages
       , module DBus.Message
       ) where

import Data.List
import Data.IORef
import Control.Monad
import Control.Applicative
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Posix
import System.IO.Unsafe
import Text.Printf

import DBus.Message ( ReceivedMessage(..) )
import DBus.Wire ( marshalMessage, unmarshalMessage, Endianness(..) )

import Channel
import Tools.Log

data Msg
   = Msg !ReceivedMessage !ByteString

bufferSz :: Int
bufferSz = 4096

-- Lazy stream of unmarshalled messages given input buffer. Each message is paired with its contents in form of byte buffer
unmarshaledMessages :: Channel -> IO [ Msg ]
unmarshaledMessages sock =
    do inc <- incomingData sock
       inc_ref <- newIORef inc
       recv_msgs <- unmarshaledMessages' inc_ref
       return $ recv_msgs

unmarshaledMessages' :: IORef LazyBS.ByteString -> IO [ Msg ]
unmarshaledMessages' buf_ref =
    do empty <- LazyBS.null <$> readIORef buf_ref
       if empty
          then return []
          else do -- will be filled with contents of single unmarshalled message (just the byte buffer)
                  contents_ref <- newIORef BS.empty
                  -- unmarshal byte buffer using reader
                  r <- unmarshalMessage (reader contents_ref)
                  contents <- readIORef contents_ref
                  case r of
                    Right m -> do ms <- unsafeInterleaveIO $ unmarshaledMessages' buf_ref
                                  return ( Msg m contents : ms )
                    Left er -> do warn $ printf "unmarshal error: %s" (show er)
                                  return []
    where
      reader contents sz =
          do -- get the buffer with incoming data
             dat <- readIORef buf_ref
             -- get the sz bytes in x, rest in xs
             let (x,xs) = LazyBS.splitAt (fromIntegral sz) dat
             -- cut the beginning bytes out of the incoming buffer
             writeIORef buf_ref xs
             -- append the beginning bytes to current message contents
             modifyIORef contents (\current -> BS.append current (compactChunks x))
             -- and finally return the begininng bytes to demarshaller
             return x

compactChunks :: LazyBS.ByteString -> ByteString
compactChunks b = foldl' BS.append BS.empty (LazyBS.toChunks b)

-- Lazy chunks of incoming data, terminating on EOF or error
incomingData :: Channel -> IO LazyBS.ByteString
incomingData sock =
    ( do chunk <- recv sock bufferSz
         when ( BS.null chunk ) $ E.throw (EOF sock)
         other_chunks <- unsafeInterleaveIO $ incomingData sock
         return $ LazyBS.append (LazyBS.fromChunks [chunk]) other_chunks
    ) `E.catch` err
  where
    err :: E.SomeException -> IO LazyBS.ByteString
    err x = do _err $ E.fromException x
               return LazyBS.empty
    _err (Just (EOF s)) = debug $ printf "%s on %s" (show (EOF s)) (show sock)
    _err (Just ex) = warn $ printf "%s on %s" (show ex) (show sock)
    _err Nothing = return ()
