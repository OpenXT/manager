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

module UpdateMgr.Error
    ( UpdateMgrError
    , LocalErr (..)
    , localE
    ) where

import Rpc.Core
import UpdateMgr.Types

data LocalErr = InternalError String
              | FileNotFound URL
              | DownloadFailed URL
              | DownloadWasCancelled
              | MalformedMetadata
              | MissingMetadata
              | UpdateNotApplicable
              | AlreadyUpToDate
              | BackgroundOpAlreadyRunning
              | GuestVmsRunning
              | CorruptFile FilePath
              | CannotResolveHost URL
              | CannotConnectHost URL
              | FailedSignatureVerification
              | UpgradesPolicyDisabled
              | NoRepositories URL
                -- Todo: Add reason, or so.
              | Forbidden

    deriving (Eq,Show)

data UpdateMgrError = UpdateMgrError {
      remoteErr :: Maybe RemoteErr
    , localErr  :: Maybe LocalErr }

instance IsRemoteError UpdateMgrError where
    fromRemoteErr call remote_err = UpdateMgrError (Just remote_err) Nothing
    toRemoteErr                   = remoteErr

instance Show UpdateMgrError where
    show (UpdateMgrError (Just remote) Nothing) = show remote
    show (UpdateMgrError Nothing (Just local))  = show (code local) ++ ":" ++ message local
    show _ = "?"

localE :: LocalErr -> UpdateMgrError
localE e = UpdateMgrError Nothing (Just e)

message :: LocalErr -> String
message (InternalError m) = "internal error: " ++ m
message (DownloadFailed url) = "download of URL " ++ show url ++ " failed"
message (DownloadWasCancelled) = "download was cancelled"
message MissingMetadata = "update metadata is missing"
message MalformedMetadata = "malformed update metadata"
message UpdateNotApplicable = "update is not applicable to platform's XenClient version"
message AlreadyUpToDate = "system is already up to date"
message BackgroundOpAlreadyRunning = "another background operation is already running"
message GuestVmsRunning = "cannot perform update while guest vms are running"
message (FileNotFound url) = "file not found " ++ url
message (CorruptFile path) = "corrupt file " ++ path
message (CannotResolveHost p) = "cannot resolve host " ++ p
message (CannotConnectHost p) = "cannot connect host " ++ p
message FailedSignatureVerification = "failed signature verification"
message UpgradesPolicyDisabled = "upgrades disabled by policy settings"
message (NoRepositories url) = "The file at URL " ++ show url ++ " does not contain any links to repositories."
message Forbidden = "Forbidden"

code :: LocalErr -> Int
code (InternalError m) = 301
code (DownloadFailed url) = 302
code (DownloadWasCancelled) = 303
code MissingMetadata = 304
code MalformedMetadata = 305
code UpdateNotApplicable = 306
code BackgroundOpAlreadyRunning = 307
code GuestVmsRunning = 308
code (CorruptFile _) = 309
code AlreadyUpToDate = 310
code (FileNotFound _) = 311
code (CannotResolveHost _) = 312
code (CannotConnectHost _) = 313
code FailedSignatureVerification = 314
code UpgradesPolicyDisabled = 315
