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

{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
module Tools.Log where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Exception as E
import System.Posix.Syslog
import System.Posix.IO

class Log m where
    debug :: String -> m ()
    warn  :: String -> m ()
    fatal :: String -> m ()
    info  :: String -> m ()

instance (MonadIO m) => Log m where
    debug = liftIO . syslog Debug
    warn  = liftIO . syslog Warning
    fatal = liftIO . syslog Error
    info  = liftIO . syslog Info
