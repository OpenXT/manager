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

{-# LANGUAGE Rank2Types, ScopedTypeVariables, ImpredicativeTypes, DatatypeContexts, ExistentialQuantification #-}
module RulesCache (dropCache, test, testAugmented, mkRulesCache, RulesCache()) where
import qualified Data.Map as M
import Control.Concurrent.MVar.Lifted
import Control.Applicative
import qualified Control.Exception.Lifted as E
import Control.Monad.Trans
import Control.Monad
import Control.Arrow

import Tools.Misc
import Tools.Log
import Tools.Db

import Rules
import Types
import Settings
import RulesParser
import RpcProxyM
import Rpc.Core

data RulesCachePure = RulesCachePure { global :: Maybe [Rule]
                                     , perUuid :: M.Map Uuid [Rule]
                                     }
emptyCache :: RulesCachePure
emptyCache = RulesCachePure { global = Nothing, perUuid = M.empty }

data RulesCache = RulesCache { dropCache :: forall m. MonadIO m => m ()
                             , test :: forall a m. (Artefact a, MonadIO m) => a -> RuleSubject -> m Bool
                             , testAugmented :: forall a m. (Artefact a, MonadIO m) =>
                               (Rule -> m Rule ) -> a -> RuleSubject -> m Bool}

mkRulesCache :: Settings -> RpcProxy RulesCache
mkRulesCache settings =
    do cache <- newMVar emptyCache
       runner <- mkRunner
       return RulesCache { dropCache = liftIO $ dropCache' cache
                         , test = testAugmented' settings cache runner return
                         , testAugmented = testAugmented' settings cache runner }
  where mkRunner :: RpcProxy (RpcProxy a ->  IO a)
        mkRunner = (\action -> (<=<) (either (fail . show) return) . runRpcProxyM $ action)
                   <$> rpcGetContext

dropCache' :: MVar RulesCachePure -> IO ()
dropCache' mvar = () <$ swapMVar mvar emptyCache

testAugmented' :: (Artefact a, MonadIO m)
                  => Settings -> MVar RulesCachePure -> (RpcProxy [Rule] -> IO [Rule])
                  -> (Rule -> m Rule) -> a -> RuleSubject
                  -> m Bool
testAugmented' settings rulesCache runner augmenter (art :: a) (subj :: RuleSubject) =
    liftM (testPure art subj) . mapM augmenter =<<
    (liftIO . runner) (getRulesFor settings rulesCache art)

getRulesFor :: Artefact a => Settings -> MVar RulesCachePure -> a -> RpcProxy [Rule]
getRulesFor settings rulesCache art =
    modifyMVar rulesCache $ \RulesCachePure { global = global,
                                              perUuid = perUuid } ->
      do newGlobal <- maybe (readGlobalRules settings) return global
         (updateCache, specificRules) <- case sourceUuid (artefactSource art) of
             Just uuid -> case M.lookup uuid perUuid of
                 -- Cache hit.
                 Just rules -> return (id, rules)
                 -- Cache miss.
                 Nothing -> (M.insert uuid &&& id) <$> readVmSpecificRules uuid
             -- No source uuid associated with this artefact.
             Nothing -> return (id, [])

         return (RulesCachePure { global = Just newGlobal
                                , perUuid = updateCache perUuid
                                },
                 -- Later rules overrule earlier ones.  So we put the
                 -- specific rules later in the list.
                 newGlobal ++ specificRules)

readVmSpecificRules :: Uuid -> RpcProxy [Rule]
readVmSpecificRules uuid = (fmap.fmap) addUuid $ dbReadWithDefault [] path
  where path = "/vm" </> show uuid </> "rpc-firewall-rules"
        a </> b = a ++ "/" ++ b

        -- ToDo: Consider lenses.
        addUuid :: Rule -> Rule
        addUuid r@Rule {match=match} = r { match = match { domain_uuid = Just uuid }}

readGlobalRules :: Settings -> RpcProxy [Rule]
readGlobalRules settings = do
  configFileContents <- liftIO $ readFile (rulesPath settings)
  case parseRules configFileContents of
          Right rs -> return rs
          Left err -> error ("malformed " ++ show (rulesPath settings) ++ ": " ++ show err)
