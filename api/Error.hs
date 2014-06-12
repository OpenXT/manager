-- XenClient Api
-- Copyright (C) 2011 Citrix Systems, Inc.
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
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

module Error
    ( ApiError
    , LocalErr (..)
    , localE
    ) where

import Rpc.Core

-- Placeholder
type LocalErr = ()

data ApiError = ApiError {
      remoteErr :: Maybe RemoteErr
    , localErr  :: Maybe LocalErr }

instance IsRemoteError ApiError where
    fromRemoteErr call remote_err = ApiError (Just remote_err) Nothing
    toRemoteErr e                 = remoteErr e

instance Show ApiError where
    show (ApiError (Just remote) Nothing) = show remote
    show (ApiError Nothing (Just local))  = "local:" ++ message local
    show _ = "?"

localE :: LocalErr -> ApiError
localE e = ApiError Nothing (Just e)

message :: LocalErr -> String
message () = "An Error occured"
