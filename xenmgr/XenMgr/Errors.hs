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

{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

module XenMgr.Errors (
                   failHibernateFailed
                 , failCannotEditPropertiesDueToPolicy
                 , failDuplicateDevice
                 , failNoSuchDevice
                 , failCannotRemoveRunning
                 , failTooManyVms
                 , failNoSuchDisk
                 , failIncorrectDiskType
                 , failIncorrectDeviceType
                 , failActionRequiresPvAddons
                 , failPropertyIsReadonly
                 , failPropertyIsWriteonly
                 , failNotGatheringDiagnostics
                 , failDiskSha1SumDoesNotMatch
                 , failCannotStartBecauseAmtPtRunning
                 , failCannotStartBecauseOemFeaturesRunning
                 , failActionSuppressedByPolicy
                 , failCannotChangePropertyWhileHibernated
                 , failNoSuchNic
                 , failIO
                 , failResumeFromSleep
                 , failVmNotReady
                 , failGuestNotUsingRpcAgent
                 , failSleepTimeout
                 , failHibernateTimeout
                 , failShutdownTimeout
                 , failDeviceAlreadyPassedThrough
                 , failGraphicsCantBePassedAsSecondaryAdapter
                 , failNetworkDomainNotRunning
                 , failRuleParseError
                 , failUnknownGpu
                 , failCannotSleepInTXTMeasuredLaunch
                 , failUnsupportedLanguage
                 , failCryptoKeyAlreadyExists
                 , failVhdKeyAlreadySet
                 , failNotEnoughMemory
                 , failTimeoutWaitingForNetworkDaemon
                 , failNoSuchFile
                 , failNetworkDisconnected
                 , failInvalidCaseId
                 , failInvalidCredentials
                 , failTaasTerms
                 , prettifyRemoteError
                 , XmError
                 ) where

import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Typeable
import qualified Data.Text.Lazy as T
import Text.Regex.Posix

import Rpc.Core

data XmError = RError RpcCall RemoteErr -- remote
             | XError ErrorCode String  -- local
               deriving Typeable

type ErrorCode = Int

instance Exception XmError

instance IsRemoteError XmError where
    fromRemoteErr = RError
    toRemoteErr (RError _ dbusE) = Just dbusE
    toRemoteErr _ = Nothing

instance Show XmError where
    show e@(XError code s) = show code ++ ":" ++ s
    show e@(RError _ _) = prettifyRemoteError e

failNotEnoughMemory :: (MonadError XmError m) => m a
failNotEnoughMemory = throwError $ XError 101 "Not enough memory!"

-- List of literate errors thrown to upper layers (UI)
failHibernateFailed :: (MonadError XmError m) => m a
failHibernateFailed = throwError $ XError 201 "Failed to hibernate VM"

failCannotEditPropertiesDueToPolicy :: (MonadError XmError m) => m a
failCannotEditPropertiesDueToPolicy = throwError $ XError 202 "Editing of properties is disabled by this VM's active policy"

failNoSuchDevice :: (MonadError XmError m) => m a
failNoSuchDevice    = throwError $ XError 203 "No such device"

failCannotRemoveRunning :: (MonadError XmError m) => m a
failCannotRemoveRunning = throwError $ XError 204 "Cannot remove running VM"

failTooManyVms :: (MonadError XmError m) => m a
failTooManyVms = throwError $ XError 205 "Too many VMS"

failNoSuchDisk :: (MonadError XmError m) => m a
failNoSuchDisk = throwError $ XError 206 "No such disk"

failIncorrectDiskType :: (MonadError XmError m) => m a
failIncorrectDiskType = throwError $ XError 207 "Incorrect disk type"

failIncorrectDeviceType :: (MonadError XmError m) => m a
failIncorrectDeviceType = throwError $ XError 208 "Incorrect device type"

failDuplicateDevice :: (MonadError XmError m) => m a
failDuplicateDevice = throwError $ XError 212 "Failed to add a disk, device ID is duplicated"

failActionRequiresPvAddons :: (MonadError XmError m) => m a
failActionRequiresPvAddons = throwError $ XError 213 "Performing this action requires PV addons installed inside VM"

failPropertyIsReadonly :: (MonadError XmError m) => m a
failPropertyIsReadonly = throwError $ XError 214 "Property is read-only"

failPropertyIsWriteonly :: (MonadError XmError m) => m a
failPropertyIsWriteonly = throwError $ XError 215 "Property is write-only"

failNotGatheringDiagnostics :: (MonadError XmError m) => m a
failNotGatheringDiagnostics = throwError $ XError 216 "Currently not gathering diagnostic information"

failDiskSha1SumDoesNotMatch :: (MonadError XmError m) => m a
failDiskSha1SumDoesNotMatch = throwError $ XError 217 "Disk SHA1 sum does not match the expected one!"

failCannotStartBecauseAmtPtRunning :: (MonadError XmError m) => m a
failCannotStartBecauseAmtPtRunning = throwError $ XError 219 "Cannot start VM - another VM with AMT passthrough is already running"

failActionSuppressedByPolicy :: (MonadError XmError m) => m a
failActionSuppressedByPolicy = throwError $ XError 220 "Action suppressed by active policy settings"

failCannotChangePropertyWhileHibernated :: (MonadError XmError m) => m a
failCannotChangePropertyWhileHibernated = throwError $ XError 221 "Cannot change VM property while it is in hibernated state"

failNoSuchNic :: (MonadError XmError m) => m a
failNoSuchNic = throwError $ XError 222 "No such NIC"

failIO :: (MonadError XmError m) => String -> m a
failIO msg = throwError $ XError 223 ("IO error: " ++ msg)

failResumeFromSleep :: (MonadError XmError m) => m a
failResumeFromSleep = throwError $ XError 224 "Failed to resume vm from sleep"

failVmNotReady :: (MonadError XmError m) => m a
failVmNotReady = throwError $ XError 225 "Cannot complete requested action because VM is not ready - possibly still under construction"

failGuestNotUsingRpcAgent :: (MonadError XmError m) => m a
failGuestNotUsingRpcAgent = throwError $ XError 226 "Guest VM is not configured to use rpc agent"

failSleepTimeout :: (MonadError XmError m) => m a
failSleepTimeout = throwError $ XError 227 "Timeout waiting for guest to enter sleep state"

failHibernateTimeout :: (MonadError XmError m) => m a
failHibernateTimeout = throwError $ XError 228 "Timeout waiting for guest to enter hibernated state"

failShutdownTimeout :: (MonadError XmError m) => m a
failShutdownTimeout = throwError $ XError 229 "Timeout waiting for guest to enter shutdown state"

failDeviceAlreadyPassedThrough :: String -> (MonadError XmError m) => m a
failDeviceAlreadyPassedThrough dev = throwError $ XError 230 ("Device '" ++ dev ++ "' is currently being passed through to another VM.")

failGraphicsCantBePassedAsSecondaryAdapter :: (MonadError XmError m) => m a
failGraphicsCantBePassedAsSecondaryAdapter = throwError $ XError 231 "Graphics card cannot be passed through as secondary adapter"

failNetworkDomainNotRunning :: (MonadError XmError m) => m a
failNetworkDomainNotRunning = throwError $ XError 232 "Network backend domain is not running"

failCannotStartBecauseOemFeaturesRunning :: (MonadError XmError m) => m a
failCannotStartBecauseOemFeaturesRunning = throwError $ XError 233 "Cannot stat VM - another VM with oem acpi features is already running"

failUnknownGpu :: (MonadError XmError m) => String -> m a
failUnknownGpu gpu = throwError $ XError 234 ("Unknown GPU " ++ show gpu)

failCannotSleepInTXTMeasuredLaunch :: (MonadError XmError m) => m a
failCannotSleepInTXTMeasuredLaunch = throwError $ XError 235 ("Host sleep is disabled while in TXT measured launch")

failUnsupportedLanguage :: (MonadError XmError m) => m a
failUnsupportedLanguage = throwError $ XError 236 "Unsupported language"

failRuleParseError :: (MonadError XmError m) => m a
failRuleParseError = throwError $ XError 237 "Rule parse error"

failCryptoKeyAlreadyExists :: (MonadError XmError m) => m a
failCryptoKeyAlreadyExists = throwError $ XError 238 "Encryption key already exists"

failVhdKeyAlreadySet :: (MonadError XmError m) => m a
failVhdKeyAlreadySet = throwError $ XError 239 "VHD file already contains encryption key"

failTimeoutWaitingForNetworkDaemon :: (MonadError XmError m) => m a
failTimeoutWaitingForNetworkDaemon = throwError $ XError 240 "Timeout waiting for network daemon to become ready"

failNoSuchFile :: (MonadError XmError m) => m a
failNoSuchFile    = throwError $ XError 242 "Supplied file doesn't exist"

failNetworkDisconnected :: (MonadError XmError m) => m a
failNetworkDisconnected    = throwError $ XError 243 "Network issue detected, please check your internet connection"

failInvalidCaseId :: (MonadError XmError m) => m a
failInvalidCaseId    = throwError $ XError 244 "A generic error occured, please check your case ID"

failInvalidCredentials :: (MonadError XmError m) => m a
failInvalidCredentials    = throwError $ XError 245 "User unauthorized, please check your MyCitrix credentials"

failTaasTerms :: (MonadError XmError m) => m a
failTaasTerms    = throwError $ XError 246 "User has not agreed to the latest version of the legal terms"

prettifyRemoteError :: XmError -> String
prettifyRemoteError e@(XError _ _) = show e
prettifyRemoteError (RError call dbus_err) =
    if "org.xen.vm" `elem` (inits i)
       then prettifyXenvmError dbus_err
       else show dbus_err
  where
    i = callInterface call

prettifyXenvmError :: RemoteErr -> String
prettifyXenvmError err =
    case remoteErrorBody err of
      [errMV] -> case fromVariant errMV of
                   Just message -> case convXenvmError message of
                                     Just m  -> m
                                     Nothing -> message
                   _            -> show err
      _       -> show err



data ErrorConv = ErrorConv Pattern Code ConvFun
type Pattern   = String
type Code      = Int
type ConvFun   = [String] -> String

plain :: String -> [String] -> String
plain msg _ = msg

xenvmErrorTable :: [ ErrorConv ]
xenvmErrorTable =
    [ ErrorConv "Not_enough_free_memory" 101 (plain "Not enough free memory!")
    , ErrorConv "Could not allocate memory for HVM guest" 101 (plain "Not enough free memory!")
    , ErrorConv "Vm_bad_state" 102 (plain "VM is in unexpected state!")
    , ErrorConv "Vm_shutdown_wrong_reason" 103 (plain "VM didn't shutdown as expected.")
    , ErrorConv "Vm_didnt_shutdown" 104 (plain "VM didn't shutdown!")
    , ErrorConv "Vm_is_already_created" 105 (plain "VM is already created!")
    , ErrorConv "RequiresMoreCapabilities\\(\"(.*)\"\\)" 106 (\match -> let [descr] = match in descr)
    , ErrorConv "Device.Tap2.Mount_failure" 107 (plain "One of VM disks failed to mount")
    , ErrorConv "exception:(.*)$" 199 (\match -> let [descr] = match in descr)
    ]

convXenvmError :: String -> Maybe String
convXenvmError str =
    foldl' f Nothing xenvmErrorTable
  where
    f acc (ErrorConv pat code trans)
      | acc == Nothing && str =~ pat   = let (_,_,_,grps) = str =~ pat :: (String,String,String,[String]) in
                                         Just $ show (XError code (trans grps))
      | otherwise                      = acc



