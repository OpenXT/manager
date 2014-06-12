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

{-# LANGUAGE NoMonomorphismRestriction #-}
module Vm.Policies (
                     policiesEnforce
                   , policiesRetrieve

                   , policyQueryCdAccess
                   , policyQueryCdRecording
                   , policyQueryAudioAccess
                   , policyQueryAudioRecording
                   , policyQueryWiredNetworking
                   , policyQueryWifiNetworking
                   , policyQueryModifyVmSettings
                   , policySetCdAccess
                   , policySetCdRecording
                   , policySetAudioAccess
                   , policySetAudioRecording
                   , policySetWiredNetworking
                   , policySetWifiNetworking
                   , policySetModifyVmSettings

                   , policyQueryVmCreation
                   , policyQueryVmDeletion
                   , policyQueryOtaUpgrades
                   , policySetVmCreation
                   , policySetVmDeletion
                   , policySetOtaUpgrades

                   , policyQueryCdExclusive
                   , policySetCdExclusive
                   , policyQueryPrintScreen
                   , policySetPrintScreen

                   ) where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Tools.Misc
import XenMgr.Rpc
import Vm.Types
import XenMgr.Db
import XenMgr.Notify

-- Effective policies
data PolicyType  = CdAccess | CdRecording | AudioAccess | AudioRecording | WiredNetworking | WifiNetworking | ModifyVmSettings
                 deriving (Eq, Ord)
-- Spec is a location in DB + default value
type PolicySpec  = (DBLocation, PolicyValue)
-- Policy for a VM is a type + current value
data Policy      = VmPolicy PolicyType PolicyValue
-- Value can only be a bool for now
data PolicyValue = BoolV Bool
                 deriving (Eq)

-- Policy value is placed in database under a specific node
type DBLocation  = String

instance Marshall PolicyValue where
    dbRead p            = dbRead p >>= return . BoolV
    dbWrite p (BoolV v) = dbWrite p v

policySpecs :: M.Map PolicyType PolicySpec
policySpecs = M.fromList $
              [ (CdAccess        , ("policies/cd-access", BoolV True))
              , (CdRecording     , ("policies/cd-rec", BoolV True))
              , (AudioAccess     , ("policies/audio-access", BoolV True))
              , (AudioRecording  , ("policies/audio-rec", BoolV True))
              , (WiredNetworking , ("policies/wired-networking", BoolV True))
              , (WifiNetworking  , ("policies/wireless-networking", BoolV True))
              , (ModifyVmSettings, ("policies/modify-vm-settings", BoolV True)) ]

policyTypes :: [PolicyType]
policyTypes = M.keys policySpecs

defaultValue :: PolicyType -> PolicyValue
defaultValue t = let Just (_,v) = t `M.lookup` policySpecs in v

defaultPolicies :: [Policy]
defaultPolicies =
    map p (M.toList policySpecs)
  where
    p (t, (_, v)) = VmPolicy t v

vmPolicyNode :: Uuid -> PolicyType -> DBLocation
vmPolicyNode uuid t =
    "/vm/" ++ show uuid ++ "/" ++ n
  where
    Just (n,_) = t `M.lookup` policySpecs

-- To string blob
pack :: [Policy] -> String
pack policies =
    concat
    . intersperse ";"
    . filter nonEmpty
    . map stringify
    $ policies
  where
    nonEmpty = (/=) ""
    stringify (VmPolicy CdAccess (BoolV False))         = "block-cd-access"
    stringify (VmPolicy CdRecording (BoolV False))      = "block-cd-rec"
    stringify (VmPolicy AudioAccess (BoolV False))      = "block-audio-access"
    stringify (VmPolicy AudioRecording (BoolV False))   = "block-audio-rec"
    stringify (VmPolicy WifiNetworking (BoolV False))   = "block-wireless-networking"
    stringify (VmPolicy WiredNetworking (BoolV False))  = "block-wired-networking"
    stringify (VmPolicy ModifyVmSettings (BoolV False)) = "block-modify-vm-settings"
    stringify _ = ""

-- From string blob
unpack :: String -> [Policy]
unpack string =
    overlay defaultPolicies policies
  where
    policies :: [Policy]
    policies =
        map fromJust
      . filter isJust
      . map unstringify
      . split ';'
      $ string

    unstringify "block-cd-access"           = Just $ VmPolicy CdAccess (BoolV False)
    unstringify "block-cd-rec"              = Just $ VmPolicy CdRecording (BoolV False)
    unstringify "block-audio-access"        = Just $ VmPolicy AudioAccess (BoolV False)
    unstringify "block-audio-rec"           = Just $ VmPolicy AudioRecording (BoolV False)
    unstringify "block-wireless-networking" = Just $ VmPolicy WifiNetworking (BoolV False)
    unstringify "block-wired-networking"    = Just $ VmPolicy WiredNetworking (BoolV False)
    unstringify "block-modify-vm-settings"  = Just $ VmPolicy ModifyVmSettings (BoolV False)
    unstringify _                           = Nothing

-- Overlay default policies with a specified set of policies, retaining default values if not present in given set
overlay :: [Policy] -> [Policy] -> [Policy]
overlay defaults policies =
    policies ++ (defaults `minus` policies)
  where
    toM policies           = M.fromList $ map entry policies
    toL m                  = map snd $ M.toList m
    entry p@(VmPolicy t v) = (t,p)
    xs `minus` ys          = toL $ (toM xs) `M.difference` (toM ys)

fromDatabase :: Uuid -> Rpc [Policy]
fromDatabase uuid =
    sequence $ map get policyTypes
  where
    get t =
        dbMaybeRead (vmPolicyNode uuid t) >>= f where f Nothing  = return $ VmPolicy t (defaultValue t)
                                                      f (Just v) = return $ VmPolicy t v

toDatabase :: Uuid -> [Policy] -> Rpc ()
toDatabase uuid policies =
    mapM_ set policies
  where
    set (VmPolicy t v) = dbWrite (vmPolicyNode uuid t) v

policiesRetrieve :: Uuid -> Rpc String
policiesRetrieve uuid =
    fromDatabase uuid >>= return . pack

policiesEnforce :: Uuid -> String -> Rpc ()
policiesEnforce uuid str =
    toDatabase uuid (unpack str) >> notifyVmConfigChanged uuid

queryVm :: Uuid -> PolicyType -> Rpc PolicyValue
queryVm uuid t =
    dbMaybeRead (vmPolicyNode uuid t) >>= f where f Nothing  = return $ defaultValue t
                                                  f (Just v) = return $ v

unp (BoolV v) = v
policyQueryCdAccess         uuid = unp <$> queryVm uuid CdAccess
policyQueryCdRecording      uuid = unp <$> queryVm uuid CdRecording
policyQueryAudioAccess      uuid = unp <$> queryVm uuid AudioAccess
policyQueryAudioRecording   uuid = unp <$> queryVm uuid AudioRecording
policyQueryWiredNetworking  uuid = unp <$> queryVm uuid WiredNetworking
policyQueryWifiNetworking   uuid = unp <$> queryVm uuid WifiNetworking
policyQueryModifyVmSettings uuid = unp <$> queryVm uuid ModifyVmSettings

policySetCdAccess uuid v = dbWrite (vmPolicyNode uuid CdAccess) v >> notifyVmConfigChanged uuid
policySetCdRecording uuid v = dbWrite (vmPolicyNode uuid CdRecording) v >> notifyVmConfigChanged uuid
policySetAudioAccess uuid v = dbWrite (vmPolicyNode uuid AudioAccess) v >> notifyVmConfigChanged uuid
policySetAudioRecording uuid v = dbWrite (vmPolicyNode uuid AudioRecording) v >> notifyVmConfigChanged uuid
policySetWiredNetworking uuid v = dbWrite (vmPolicyNode uuid WiredNetworking) v >> notifyVmConfigChanged uuid
policySetWifiNetworking uuid v = dbWrite (vmPolicyNode uuid WifiNetworking) v >> notifyVmConfigChanged uuid
policySetModifyVmSettings uuid v = dbWrite (vmPolicyNode uuid ModifyVmSettings) v >> notifyVmConfigChanged uuid

policyQueryVmCreation = not <$> dbReadWithDefault False "/vm-create-disallowed"
policyQueryVmDeletion = not <$> dbReadWithDefault False "/vm-delete-disallowed"
policyQueryOtaUpgrades = not <$> dbReadWithDefault False "/ota-upgrades-disallowed"

policySetVmCreation True  = dbRm "/vm-create-disallowed"
policySetVmCreation False = dbWrite "/vm-create-disallowed" True
policySetVmDeletion True  = dbRm "/vm-delete-disallowed"
policySetVmDeletion False = dbWrite "/vm-delete-disallowed" True
policySetOtaUpgrades True = dbRm "/ota-upgrades-disallowed"
policySetOtaUpgrades False= dbWrite "/ota-upgrades-disallowed" True

policyQueryCdExclusive     = dbReadWithDefault True "/cd-exclusive"
policySetCdExclusive True  = dbWrite "/cd-exclusive" True
policySetCdExclusive False = dbWrite "/cd-exclusive" False

policyQueryPrintScreen uuid       = not <$> dbReadWithDefault False ("/vm/" ++ show uuid ++ "/policies/print-screen-disallowed")
policySetPrintScreen   uuid True  = dbRm ("/vm/" ++ show uuid ++ "/policies/print-screen-disallowed")
policySetPrintScreen   uuid False = dbWrite ("/vm/" ++ show uuid ++ "/policies/print-screen-disallowed") True
