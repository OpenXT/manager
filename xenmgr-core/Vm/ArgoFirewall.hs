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

{-# LANGUAGE ScopedTypeVariables,PatternGuards #-}
module Vm.ArgoFirewall
       ( DomainMatch (..)
       , PortMatch (..)
       , Endpoint (..)
       , Rule (..)
       , ActiveRule (..)
       , ActiveVm (..)
       , Change (..)
       , ReduceContext (..)

       , parseRule
       , ruleToString

       , reworkRules
       , applyActiveRule
       , unapplyActiveRule
       , applyChangeset
       , inverse
       , reduce
       , changeset
       ) where


import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Map (Map)
import Data.String
import Control.Applicative ( (<$>) )
import Control.Monad
import Data.List ( foldl' )
import Data.String ( fromString )
import qualified Data.Text.Lazy as TL
import Text.ParserCombinators.Parsec
--import XenMgr.Db
import Tools.Misc
import Tools.Log
import Tools.Process
import Tools.Db

import Vm.Uuid
import Data.Int

type DomainID = Int32

-- TODO: generalise to be able to match on any vm properties
data DomainMatch = Any
                 | Dom0
                 | Domid DomainID
                 | Vm Uuid
                 | Myself
                 | MyStubdom
                 | MyselfIfSeamlessTrafficVm
                 | ByType String
                 | ByName String
                 | SeamlessTrafficVm deriving ( Eq,Show )

data PortMatch = AnyPort | PortNum Int deriving ( Eq,Show )
data Endpoint = DomainEnd DomainID | AnyEndpoint deriving ( Eq,Show )

-- high level, uuid based rule
data Rule =
     Open { from :: DomainMatch
          , to :: DomainMatch
          , fromPort :: PortMatch
          , toPort :: PortMatch }
     deriving (Eq, Show)

instance Marshall Rule where
    dbWrite p r = dbWrite p (ruleToString r)
    dbRead  p   = do
      str <- dbRead p
      case parseRule str of
        Nothing -> error $ "failed to parse argo firewall rule at " ++ show p
        Just r  -> return r

-- low level rule actually applied to viptables
data ActiveRule =
     ActiveOpen { activeFrom :: Endpoint
                , activeTo :: Endpoint
                , activeFromPort :: PortMatch
                , activeToPort :: PortMatch }
     deriving (Eq, Show)

-- running vm
data ActiveVm =
     ActiveVm { vmDomID :: DomainID
              , vmStubdomID :: Maybe DomainID
              , vmUuid :: Uuid 
              , vmMatchType :: String
              , vmMatchName :: String }
     deriving (Eq, Show)

-- firewall action
data Change = Add ActiveRule | Remove ActiveRule

data ReduceContext =
     ReduceContext { myself :: Uuid
                   , seamlessVms :: [Uuid]
                   , runningVms :: [ActiveVm] }

-- opposite direction traffic rule
inverse :: Rule -> Rule
inverse (Open f t fp tp) = Open t f tp fp

-- reduce set of rules into active rules, given set of running vms
reduce :: ReduceContext -> [Rule] -> [ActiveRule]
reduce (ReduceContext myself seamless vms) rules =
    filter (not . internal) . concatMap reduce_one $ rules
  where
    -- filter out useless internal rules (self->self)
    internal rule = activeFrom rule == activeTo rule

    -- uuid -> domid mapping
    vm_map :: Map Uuid DomainID
    vm_map = Map.fromList $ map (\(ActiveVm domid stubdom uuid _ _) -> (uuid,domid)) vms

    stubdom_map :: Map Uuid (Maybe DomainID)
    stubdom_map = Map.fromList $ map(\(ActiveVm domid stubdom uuid _ _) -> (uuid,stubdom)) vms

    reduce_one :: Rule -> [ActiveRule]
    reduce_one r@(Open f t fp tp) =
        do f' <- from_ds
           t' <- to_ds
           return $ ActiveOpen f' t' fp tp
        where
          from_ds = reduce_domain f
          to_ds   = reduce_domain t

    reduce_domain :: DomainMatch -> [Endpoint]
    reduce_domain Any  = [AnyEndpoint]
    reduce_domain Dom0 = [DomainEnd 0]
    reduce_domain (Domid id) = [DomainEnd id]
    reduce_domain MyStubdom
        | Just (Just domid) <- myself `Map.lookup` stubdom_map
          = [DomainEnd domid]
        | otherwise
          = []

    reduce_domain (Vm uuid)
        | Just domid <- uuid `Map.lookup` vm_map  = [DomainEnd domid]
        | otherwise                               = []
    reduce_domain Myself
        | Just domid <- myself `Map.lookup` vm_map = [DomainEnd domid]
        | otherwise                                = []
    reduce_domain MyselfIfSeamlessTrafficVm
        | Just domid <- myself `Map.lookup` vm_map
        , myself `elem` seamless                   = [DomainEnd domid]
        | otherwise                                = []
    reduce_domain SeamlessTrafficVm = do
      sms <- seamless
      case sms `Map.lookup` vm_map of
        Just domid -> return $ DomainEnd domid
        Nothing    -> []

    reduce_domain (ByType t) = [DomainEnd (vmDomID vm) | vm <- vms, vmMatchType vm == t]
    reduce_domain (ByName n) = [DomainEnd (vmDomID vm) | vm <- vms, vmMatchName vm == n]

-- compute actions required to bring firewall from state a to b
changeset :: [ActiveRule] -> [ActiveRule] -> [Change]
changeset state_a state_b =
    map Remove (state_a `differenceList` state_b)
 ++ map Add    (state_b `differenceList` state_a)

viptablesParams :: ActiveRule -> String
viptablesParams r =
    concat . intersperse " " . filter (not . null) $
               [ from_s (activeFrom r)
               , to_s (activeTo r)
               , fromp_s (activeFromPort r)
               , top_s (activeToPort r)
               , "-j ACCEPT" ]
    where
      from_s (DomainEnd id) = "--dom-in " ++ show id
      from_s AnyEndpoint    = ""
      to_s   (DomainEnd id) = "--dom-out " ++ show id
      to_s   AnyEndpoint    = ""

      fromp_s (PortNum p) = "--port-in " ++ show p
      fromp_s AnyPort  = ""
      top_s   (PortNum p) = "--port-out " ++ show p
      top_s   AnyPort  = ""

applyChangeset :: [Change] -> IO ()
applyChangeset = mapM_ apply_change where
    apply_change (Add r) = applyActiveRule r
    apply_change (Remove r) = unapplyActiveRule r

applyActiveRule :: ActiveRule -> IO ()
applyActiveRule r = let cmd = "viptables -I 1 " ++ viptablesParams r in
                    info ("+argo-rule " ++ cmd) >> safeSpawnShell cmd >> return ()

unapplyActiveRule :: ActiveRule -> IO ()
unapplyActiveRule r = let cmd = "viptables -D " ++ viptablesParams r in
                      info ("-argo-rule " ++ cmd) >> safeSpawnShell cmd >> return ()

-- start firewall in deny all mode
startToDenyAll :: IO ()
startToDenyAll = do
    safeSpawnShell "viptables -F"
    safeSpawnShell "viptables -A -j REJECT"
    return ()

reworkRules :: IO a -> IO a
reworkRules action = do
    safeSpawnShell "viptables -F"
    safeSpawnShell "viptables -A -j ACCEPT"
    r <- action
    safeSpawnShell "viptables -D -j ACCEPT"
    -- allow dom0 -> dom0 (bl**dy argoproxy)
    safeSpawnShell "viptables -A --dom-in 0 --dom-out 0 -j ACCEPT"
    safeSpawnShell "viptables -A -j REJECT"
    return r

ruleToString :: Rule -> String
ruleToString (Open f t fp tp) =
    domain_str f ++ p_str fp ++ " -> " ++ domain_str t ++ p_str tp
    where
      domain_str Any       = "*"
      domain_str Dom0      = "0"
      domain_str (Domid id)= show id
      domain_str (Vm uuid) = show uuid
      domain_str Myself    = "myself"
      domain_str MyStubdom = "my-stubdom"
      domain_str MyselfIfSeamlessTrafficVm = "myself-if-seamless"
      domain_str SeamlessTrafficVm = "seamless"
      domain_str (ByType n) = "dom-type=" ++ n
      domain_str (ByName n) = "dom-name=" ++ n
      p_str AnyPort     = ""
      p_str (PortNum p) = ':' : (show p)


--
-- rule parser
--
tabspace_p = (char ' ' <|> tab)
whitespaces_p = many tabspace_p

hex_p =
    char 'a' <|> char 'b' <|> char 'c' <|> char 'd' <|> char 'e' <|> char 'f' <|>
    char 'A' <|> char 'B' <|> char 'C' <|> char 'D' <|> char 'E' <|> char 'F'

uuidchar_p = digit <|> hex_p <|> char '-'
uuid_p = many1 uuidchar_p

port_p =
     try ( char ':' >> whitespaces_p >> many1 digit >>= return . PortNum . read_int )
 <|> try ( char ':' >> char '*' >> return AnyPort )
 <|> ( return AnyPort )
     where read_int = read :: String -> Int

domain_p =
     ( char '*' >> return Any )
 <|> try ( string "myself-if-seamless" >> return MyselfIfSeamlessTrafficVm )
 <|> try ( string "my-stubdom" >> return MyStubdom )
 <|> try ( string "myself" >> return Myself )
 <|> try ( string "seamless" >> return SeamlessTrafficVm )
 <|> try ( string "dom-type=" >> (many1 alphaNum) >>= return . ByType )
 <|> try ( string "dom-name=" >> (many1 alphaNum) >>= return . ByName )
 <|> ( uuid_p   >>= \uuid -> case uuid of "0" -> return Dom0
                                          _   -> return $ Vm (fromString uuid) )
endpoint_p =
    do dom <- domain_p
       port <- port_p
       return (dom,port)

rule_p =
    do whitespaces_p
       (from, from_port) <- endpoint_p
       whitespaces_p
       char '-'
       char '>'
       whitespaces_p
       (to, to_port) <- endpoint_p
       whitespaces_p
       return $ Open from to from_port to_port

parseRule :: String -> Maybe Rule
parseRule str = case parse rule_p "" (map toLower str) of
                  Left _  -> Nothing
                  Right r -> Just r

