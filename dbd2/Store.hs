--
-- Copyright (c) 2013 Citrix Systems, Inc.
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

module Store where

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import System.IO.Unsafe
import System.FilePath
import System.Directory
import qualified Data.Map as M
import Data.List (partition, isSuffixOf, find)
import Path
import Tree
import Perms
import Utils

data Store = Store
    { dirty  :: Bool
    , store_ :: Tree
    , perms_ :: PermTree
    }

type StoreVar = MVar Store

otherDbFile = "/config/db"
otherPermFile = "/config/db.perms"

theStore :: StoreVar
{-# NOINLINE theStore #-}
theStore = unsafePerformIO (newMVar $ Store False Empty emptyPermTree)

data Rule = Rule
    { nodePath :: Path
    , place    :: String
    }

rules =
    [ Rule { nodePath = ["vm"], place = "/config/vms" }
    , Rule { nodePath = ["dom-store"], place = "/config/dom-store" }
    ]

isDbFile = isSuffixOf ".db"
isPermFile = isSuffixOf ".perms"

readDBFile :: FilePath -> IO Tree
readDBFile = readJSONFile
    where
        readJSONFile filename = either (const Empty) ofJSON . parseJSON <$> readFile filename

readDB :: IO Tree
readDB = do
    otherTree <- readDBFile otherDbFile
    foldM readDbRule otherTree rules
    where readDbRule tree rule = do
              (dirs,files) <- getDirsAndFiles (place rule)
              let (dbFiles,otherFiles) = partition isDbFile files
                  (_      ,binFiles)   = partition isPermFile otherFiles
              ruleTrees <- mapM (\e -> liftM ((,) (basenameNoExt e)) (readDBFile e)) dbFiles
              let binTrees = map (\f -> (basename f, FilePtr f)) binFiles
              let tree2 = foldl (mergeTree (nodePath rule)) tree ruleTrees
              let tree3 = foldl (mergeTree (nodePath rule)) tree2 binTrees
              foldM (\t d -> readDbRule t (Rule (nodePath rule ++ [d]) d)) tree3 dirs

          mergeTree rootPath acc (n,t) = Tree.inject (rootPath ++ [n]) t acc

readPermTree :: IO PermTree
readPermTree = do
    otherPerms <- readPerms otherPermFile
    foldM append (initPermTree otherPerms) rules
    where append tree rule = do
              (_,files) <- getDirsAndFiles (place rule)
              let (permsFiles,_) = partition isPermFile files
              ruleTrees <- mapM (\e -> liftM ((,) (basenameNoExt e)) (readPerms e)) permsFiles
              return $ foldl (\t (p,s) -> addPermsToTree (nodePath rule ++ [p], s) t) tree ruleTrees
          
-- | write a tree to disk, making sure it's atomic my using a mov.
writeDBFile :: FilePath -> Tree -> IO ()
writeDBFile _ (FilePtr _) = return ()
writeDBFile filename tree = writeFileSafely filename (pretty $ toJSON tree)

writePermTree (PermTree trees root) = do
    mapM_ (\(path, tree) ->
        case find (\r -> isPathPrefix (nodePath r) path) rules of
            Nothing   -> putStrLn "no rules matching"
            Just rule -> do
                let filepath = (place rule) ++ (showPath $ substractPrefix (nodePath rule) path) ++ ".perms"
                writePerms filepath tree
        ) $ M.toList trees
    writePerms otherPermFile root
    return ()

writeDB :: Bool -> IO ()
writeDB unlinkFile = modifyMVar_ theStore writeDbAndTrim
    where writeDbAndTrim st@(Store False _ _) = return st
          writeDbAndTrim (Store _ tree ptree) = do
              mapM_ (writeDbRule tree) rules
              let otherTree = foldl trimWithRule tree rules
              writeDBFile otherDbFile otherTree
              writePermTree ptree
              return $ Store False tree ptree
          writeDbRule tree rule = do
                case Tree.getSubtree (nodePath rule) tree of
                    Nodes ns -> fillDir ns (place rule)
                    _        -> return ()
          fillDir ns dirname = do
              mkdirRec dirname
              sequence_ $ map (uncurry writeDBFile . prepare) ns
              when unlinkFile doUnlink
                where prepare :: (String, Tree) -> (String, Tree)
                      prepare (name,tree) = (dirname </> (name ++ ".db"), tree)
                      doUnlink = do
                          (_, files) <- getDirsAndFiles dirname
                          let toRemove = filter (\f -> not $ elem (basenameNoExt f) nodeNames) $ filter isDbFile files
                          mapM_ removeFile toRemove
                          return ()
                      nodeNames = map fst ns
          trimWithRule tree rule =
              Tree.rm (nodePath rule) tree

-- | restore a database from disk
restore :: IO ()
restore = liftM2 (,) readDB readPermTree >>= fill
    where fill (ents,ptree) = modifyMVar_ theStore (\_ -> return $ Store False ents ptree)

-- | flush a database to disk
flush :: IO ()
flush = writeDB True

-- | check if dirty
isDirty :: IO Bool
isDirty = dirty <$> readMVar theStore

-- | modify the tree.
modifyTree :: (Tree -> Tree) -> IO ()
modifyTree f = modifyMVar_ theStore (\(Store _ tree ptree) -> return $ Store True (f tree) ptree)

-- | modify the tree.
modifyPerms :: (PermTree -> PermTree) -> IO ()
modifyPerms f = modifyMVar_ theStore (\(Store _ tree ptree) -> return $ Store True tree (f ptree))

-- | with a tree.
withTree :: (Tree -> a) -> IO a
withTree f = f . store_ <$> readMVar theStore

-- | with a permtree.
withPerms :: (PermTree -> a) -> IO a
withPerms f = f . perms_ <$> readMVar theStore

-- | check read perms
checkReadPerm :: PathCred -> IO ()
checkReadPerm (_   ,Nothing)   = return ()
checkReadPerm (path,Just uuid) = withPerms $ \pt -> ()

-- | check write perms
checkWritePerm :: PathCred -> IO ()
checkWritePerm (_   ,Nothing)   = return ()
checkWritePerm (path,Just uuid) = let p = withPerms (getPerms path)
                                   in return ()
