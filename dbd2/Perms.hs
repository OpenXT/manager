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

{-# LANGUAGE ViewPatterns #-}
module Perms
    ( Qualifier(..)
    , PermMode(..)
    , Perm(..)
    , Perms
    , PermTree(..)
    , PermPatchs(..)
    -- * IO
    , readPerms
    , writePerms
    -- * tree
    , emptyPermTree
    , initPermTree
    , addPermsToTree
    , getPerms
    , addPerm
    , rmPerm
    , chPerm
    , parsePermPatchs
    ) where

import Path
import Control.Applicative ((<$>))
import Data.Char (isDigit)
import Utils (Uuid(..), parseJSON, pretty, readFileSafely, writeFileSafely)
import Text.JSON
import qualified Data.Map as M

data Qualifier = QualDomid Int
               | QualUuid Uuid
               | QualName String
               | QualPropertyEq String String
               deriving (Eq)

data PermMode = Read
              | Write
              | ReadWrite
              deriving (Eq)

instance Show PermMode where
    show Read      = "r"
    show Write     = "w"
    show ReadWrite = "b"

data Perm = Perm Qualifier PermMode
          deriving (Eq)

instance Show Qualifier where
    show (QualUuid (Uuid uuid)) = uuid
    show (QualDomid domid)      = show domid
    show (QualName n)           = "@" ++ n
    show (QualPropertyEq s1 s2) = "{" ++ s1 ++ "==" ++ s2 ++ "}"

instance Show Perm where
    show (Perm qual mode) = show mode ++ ":" ++ show qual

data PermModification = PermAdd
                      | PermDel
                      deriving (Show,Eq)

-- | a permpatch is a modification to apply to a tree.
type PermPatch = (PermModification, Perm)

-- | patches to apply to a tree.
data PermPatchs = PermPatchs [PermPatch]
                deriving (Show,Eq)

type Perms = M.Map Path [Perm]

data PermTree = PermTree 
    { treePerms :: M.Map Path Perms
    , rootPerms :: Perms
    } deriving (Show,Eq)

pathFindLongest :: M.Map Path a -> Path -> Maybe (Path, a)
pathFindLongest m iniPath = loop iniPath
    where loop path
            | isNullPath path = Nothing
            | otherwise       = case M.lookup path m of
                                     Nothing -> loop (parentPath path)
                                     Just a  -> Just (path, a)

readPerms :: FilePath -> IO Perms
readPerms filepath = either (const M.empty) toPerms . parseJSON <$> readFileSafely "{}" filepath
    where toPerms :: JSValue -> Perms
          toPerms (JSObject (fromJSObject -> ents)) = foldl add M.empty ents
          toPerms _                                 = M.empty

          -- very lenient parsing
          add :: Perms -> (String, JSValue) -> Perms
          add acc (k,JSArray a) = M.insert (pathOf k) (reverse $ foldl accPerm [] a) acc
          add acc (k,JSString (fromJSString -> s)) = case parsePerm s of
                                                          Nothing -> acc
                                                          Just p  -> M.insert (pathOf k) [p] acc
          add acc _     = acc

          -- accumulate only string that are valid permission string
          accPerm :: [Perm] -> JSValue -> [Perm]
          accPerm acc (JSString (fromJSString -> s)) = case parsePerm s of 
                                                            Nothing -> acc
                                                            Just p  -> p : acc
          accPerm acc _ = acc

writePerms :: FilePath -> Perms -> IO ()
writePerms filepath perms = writeFileSafely filepath $ pretty (JSObject (toJSObject $ map toJSON $ M.toList perms))
    where toJSON (path, ps) = case ps of
                                [x] -> (showPath path, toPermString x)
                                _   -> (showPath path, JSArray $ map toPermString ps)
          toPermString perm = JSString $ toJSString $ show perm

emptyPermTree :: PermTree
emptyPermTree = initPermTree M.empty

initPermTree :: Perms -> PermTree
initPermTree root = PermTree M.empty root

addPermsToTree :: (Path, Perms) -> PermTree -> PermTree
addPermsToTree (x,y) (PermTree trees root) = PermTree (M.insert x y trees) root

getPerms :: Path -> PermTree -> [Perm]
getPerms path (PermTree other root) =
    case pathFindLongest other path of
        Nothing             -> maybe [] snd $ pathFindLongest root path
        Just (prefix, tree) -> case pathFindLongest tree (prefix `substractPrefix` path) of
                                    Nothing       -> getPerms path (PermTree M.empty root)
                                    Just (_,perm) -> perm

addPerm :: Path -> Perm -> PermTree -> PermTree
addPerm path perm pt@(PermTree other root)
    | origPerm == patchedPerm = pt
    | otherwise = case pathFindLongest other path of
                Nothing             ->
                    let newTree = M.alter updateOrInsertPerm path root
                     in PermTree other newTree
                Just (prefix, tree) ->
                    let subpath = prefix `substractPrefix` path
                        newTree = M.alter updateOrInsertPerm subpath tree
                     in PermTree (M.insert prefix newTree other) root
    where updateOrInsertPerm _ = Just patchedPerm

          origPerm    = getPerms path pt
          patchedPerm = if perm `elem` origPerm then origPerm else perm:origPerm
 
rmPerm :: Path -> Perm -> PermTree -> PermTree
rmPerm path perm pt@(PermTree other root)
    | origPerm == patchedPerm = pt
    | otherwise = case pathFindLongest other path of
                    Nothing             ->
                        let newTree = M.alter doRemove path root
                         in PermTree other newTree
                    Just (prefix, tree) ->
                        let subpath = prefix `substractPrefix` path
                            newTree = M.alter doRemove subpath tree
                         in PermTree (M.insert prefix newTree other) root
    where doRemove _ = if null patchedPerm then Nothing else Just patchedPerm

          origPerm    = getPerms path pt
          patchedPerm = filter (/= perm) origPerm

chPerm :: Path -> PermPatchs -> PermTree -> PermTree
chPerm path (PermPatchs permpatch) pt = loop pt permpatch
    where loop permtree []     = permtree
          loop permtree (x:xs) =
               let newPT = case x of
                               (PermAdd, z) -> addPerm path z permtree
                               (PermDel, z) -> rmPerm path z permtree
                in loop newPT xs

-- permission string is one of "rwb" followed by : followed by an uuid
parsePerm s
    | isPermModeChar (s !! 0) && (s !! 1 == ':') = case parseQualifier (drop 2 s) of
                                                        Nothing   -> Nothing
                                                        Just qual -> Just (Perm qual (toPermMode $ head s))
    | otherwise = Nothing
    where
          parseQualifier :: String -> Maybe Qualifier
          parseQualifier q
            | length q == 36      = Just $ QualUuid $ Uuid q
            | (all isDigit q)     = Just $ QualDomid (read q)
            | q !! 0 == '@'       = Just $ QualName (drop 1 q)
            | q !! 0 == '{' && q !! (length q - 1) == '}' = Just $ QualPropertyEq "" "" -- FIXME
            | otherwise           = Nothing
          isPermModeChar c = elem c "rwb"
          toPermMode 'r' = Read
          toPermMode 'w' = Write
          toPermMode 'b' = ReadWrite
          toPermMode _   = error "not a valid perm mode"

parsePermPatch s
    | length s == 39 && (s !! 0 == '+') = (\p -> (PermAdd,p)) `fmap` parsePerm (tail s)
    | length s == 39 && (s !! 0 == '-') = (\p -> (PermDel,p)) `fmap` parsePerm (tail s)
    | otherwise = Nothing

parsePermPatchs = PermPatchs . loop
    where loop s
            | null s         = []
            | length s < 39  = []
            | otherwise      = let (s1, s2) = splitAt 39 s
                                in case parsePermPatch s1 of
                                        Nothing -> loop s2
                                        Just p  -> p : loop s2
