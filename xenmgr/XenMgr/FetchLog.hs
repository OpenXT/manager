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

module XenMgr.FetchLog (
                         fetchLogInitState
                       , fetchLogPrepare
                       , fetchLogChunk
                       , fetchLogEnd
                       ) where

import Control.Monad
import Control.Applicative
import Control.Exception
import Data.IORef
import Data.List
import System.IO
import System.Posix.Files

import Tools.Misc
import Tools.Log
import Tools.Process

type FetchState = IORef (Maybe Handle)
type NumLines   = Int

fetchLogInitState :: IO FetchState
fetchLogInitState = newIORef Nothing

fetchLogPrepare :: FetchState -> NumLines -> IO ()
fetchLogPrepare state numLines = do
    spawnShell $ "tail -n " ++ show numLines ++ " /var/log/messages >/tmp/messages.part"
    handle <- openFile "/tmp/messages.part" ReadMode
    writeIORef state (Just handle)

fetchLogChunk :: FetchState -> IO String
fetchLogChunk state =
    do handle <- readIORef state
       case handle of
         Nothing -> return ""
         Just h  -> getLines h 100 []
  where
    getLines h 0 lines = give lines
    getLines h n lines = do eof <- hIsEOF h
                            case eof of
                              False ->
                                  do line <- hGetLine h
                                     getLines h (n-1) (line:lines)
                              True ->
                                  do fetchLogEnd state
                                     give lines

    give = return . concat . intersperse "\n" . reverse

fetchLogEnd :: FetchState -> IO ()
fetchLogEnd state =
    do handle <- readIORef state
       case handle of
         Nothing -> return ()
         Just h  ->
                   do try (removeLink "/tmp/messages.part") :: IO (Either IOError ())
                      hClose h
                      return ()
