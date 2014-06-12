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

-- some helpers for Control.Monad.Error similar to ones in Control.Exception
module Tools.Error where

import Control.Applicative
import Control.Monad.Error

try :: (MonadError e m) => m a -> m (Either e a)
try action = (return . Right =<< action) `catchError` (\e -> return $ Left e)

finally :: (MonadError e m) => m () -> m a -> m a
finally guard action = from =<< try action where
    from (Right r) = guard >> return r
    from (Left er) = guard >> throwError er

finally' :: (MonadError e m) => m a -> m () -> m a
finally' = flip finally
