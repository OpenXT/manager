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

module Vm.Balloon
       ( balance
       , balanceToBoot
       , getTotalOveruseKib
       )
       where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Text.Printf
import Vm.DomainCore
import Vm.Queries
import Vm.Types

import XenMgr.Host
import XenMgr.Rpc
import qualified XenMgr.Connect.Xenvm as Xenvm

import Tools.XenStore
import Tools.Log
import Tools.IfM

data VmMem
   = VmMem { minKib :: Integer
           , targetKib :: Integer }

type MemMap = Map DomainID VmMem
type OveruseMap = Map DomainID Integer

-- | current vm memory usages
memMap :: Rpc MemMap
memMap = foldM add Map.empty =<< getVms where
  add mmap vm
    = whenDomainID mmap vm $ \domid ->
        Map.insert <$> pure domid <*> v <*> pure mmap
    where v = VmMem
                <$> ( memoryMinKib' vm )
                <*> ( getVmMemoryTargetKib vm )

memoryMinKib' :: Uuid -> Rpc Integer
memoryMinKib' vm = from =<< getVmMemoryMin vm where
  from 0 = getVmMemoryTargetKib vm -- assume target if minimum not specified (i.e. no overuse)
  from x = return $ mibToKib $ fromIntegral x

-- | current vm memory overusage (above minimum value)
overuses :: MemMap -> OveruseMap
overuses = Map.map (\m -> max 0 $ targetKib m - minKib m)

-- | calculate the mem steals from vms to satisfy free mem request
stealList :: PhysInfo -> MemMap -> Integer -> Maybe [(DomainID, Integer)]
stealList pi mmap want_free_kib =
  steal need_kib (Map.toList . overuses $ mmap)
  where
    need_kib = want_free_kib - (pagesToKib $ fromIntegral $ freePages pi)
    steal needed []
      | needed > 0 = Nothing -- cannot satisfy the request
      | otherwise  = return []
    steal needed ((domid,overuse):vms)
      | needed <= 0 = return []
      | overuse > 0 = do
        let amt = min overuse needed
        xs <- steal (needed-amt) vms
        return $ (domid,amt) : xs
      | otherwise   = steal needed vms

getTargetKib :: DomainID -> Rpc Integer
getTargetKib domid
  = maybe 0 read <$> (liftIO $ xsRead ("/local/domain/" ++ show domid ++ "/memory/target"))

setTargetKib :: DomainID -> Integer -> Rpc ()
setTargetKib domid kib =
  liftIO $ xsWrite ("/local/domain/" ++ show domid ++ "/memory/target") (show kib)

describe :: PhysInfo -> String
describe i = printf "total %11d KiB, free %11d KiB, scrub %11d KiB"
             (pagesToKib $ totalPages i) (pagesToKib $ freePages i) (pagesToKib $ scrubPages i)

spinUntilFree :: Integer -> IO Bool
spinUntilFree kib = aux 10 kib where -- 1s max to get a reaction from pv driv
  aux 0 _ = warn (printf "failed to get %d KiB memory free" kib) >> return False
  aux n kib =
    do pi <- getPhysInfo
       if (kib > (pagesToKib $ fromIntegral $ freePages pi))
          then threadDelay (10^5) >> aux (n-1) kib
          else return True

balanceIter :: Integer -> Rpc Bool
balanceIter want_free_kib
  = do pi <- liftIO getPhysInfo
       let free = pagesToKib . fromIntegral $ freePages pi
           scrb = pagesToKib . fromIntegral $ scrubPages pi
       if want_free_kib <= free
          then return True
          else do
            info $ printf "rebalance: want free %d KiB, have free %d KiB, scrub %d KiB" want_free_kib free (scrb::Integer)
            achievable <- run =<< whereToStealFrom pi
            if achievable
              then do
                achieved <- liftIO $ spinUntilFree want_free_kib
                pi' <- liftIO getPhysInfo
                info $ "memory rebalance: " ++ if achieved then "OK" else "FAIL"
                info $ "before " ++ describe pi
                info $ "after  " ++ describe pi'
                return achieved
              else do
                info $ printf "rebalance: target free amount of %d KiB is not achievable" want_free_kib
                return False

    where
      run Nothing = return False
      run (Just steals) = mapM_ reduce steals >> return True
      whereToStealFrom physinfo
        = stealList
            <$> pure physinfo
            <*> memMap
            <*> pure want_free_kib
      reduce (domid,amt) =
        do t <- getTargetKib domid
           info $ printf ("reduce domain's " ++ show domid ++ " memtarget %d KiB -> %d KiB") t (t-amt)
           setTargetKib domid (t-amt)

maxRebalanceIters = 5

balance :: Integer -> Rpc Bool
balance kib = go maxRebalanceIters where
  go 0 = return False
  go n = ifM (balanceIter kib) (return True) (delay >> info "rebalance: another iteration" >> go (n-1))
  delay = liftIO $ threadDelay (10^6) -- 1s

balanceToBoot :: Uuid -> Rpc Bool
balanceToBoot vm
  = balance =<< Xenvm.requiredToBootKib vm where

getTotalOveruseKib :: Rpc Integer
getTotalOveruseKib = sum . Map.elems . overuses <$> memMap
