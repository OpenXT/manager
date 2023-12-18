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
import System.Posix.Syslog
import Foreign.C

class Log m where
    debug :: String -> m ()
    warn  :: String -> m ()
    fatal :: String -> m ()
    info  :: String -> m ()

instance (MonadIO m) => Log m where
    debug s = liftIO $ withCStringLen s (\p ->syslog (Just User) Debug p)
    warn s = liftIO $ withCStringLen s (\p ->syslog (Just User) Warning p)
    fatal s = liftIO $ withCStringLen s (\p ->syslog (Just User) Error p)
    info s = liftIO $ withCStringLen s (\p ->syslog (Just User) Info p)
