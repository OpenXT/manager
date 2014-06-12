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

module XenMgr.Testing ( TestingContext
                      , testingCreateContext
                      , testingQueueScript
                      , testingDequeueScript ) where

import Control.Concurrent
import Control.Concurrent.Chan

newtype TestingContext = TestingContext { chan :: (Chan String) }

testingCreateContext :: IO TestingContext
testingCreateContext = newChan >>= return . TestingContext

testingQueueScript :: TestingContext -> String -> IO ()
testingQueueScript c s = writeChan (chan c) s

testingDequeueScript :: TestingContext -> IO String
testingDequeueScript c = readChan (chan c)
