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
{-# LANGUAGE ScopedTypeVariables #-}
module Utils (parseJSON, pretty, mkdirRec, suffixChop, getDirsAndFiles, basenameNoExt, basename, splitOn, whileM, Uuid(..), mapOne, mapOneAppend, writeFileSafely, readFileSafely) where

import Control.Applicative ((<$>))
import Control.Monad
import Text.JSON
import System.FilePath
import System.Posix.Files
import System.Directory
import Data.List (isSuffixOf)
import qualified Control.Exception as E

data State = State Int Bool
type PP = State -> (State, ShowS)

newState :: State
newState = State 0 True

runPP :: PP -> State -> ShowS
runPP f st = snd $ f st

parseJSON :: String -> Either String JSValue
parseJSON = resultToEither . decode

pretty :: JSValue -> String
pretty val = case val of
                  JSObject _ -> runPP (p val) newState $ ""
                  JSArray _  -> runPP (p val) newState $ ""
                  _          -> error "need to be array or object at toplevel"
    where p :: JSValue -> PP
          p (JSObject (fromJSObject -> obj))
            | null obj  = ws "{}"
            | otherwise = ws "{" `c` nl `c` inc `c` printCommaSep printKV obj `c` dec `c` nl `c` ws "}"
          p (JSArray a)
            | null a    = ws "[]"
            | otherwise = ws "[" `c` nl `c` inc `c` printCommaSep p a `c` dec `c` nl `c` ws "]"
          p JSNull           = w showJSNull
          p (JSBool b)       = w (showJSBool b)
          p (JSString str)   = w (encode str ++)
          p (JSRational b r) = w (showJSRational' b r)

          printCommaSep _ []     = ws ""
          printCommaSep f (x:[]) = f x
          printCommaSep f (x:xs) = f x `c` ws "," `c` nl `c` printCommaSep f xs

          printKV (k,v) = ws (encode $ toJSString k) `c` ws ": " `c` p v

          w ss = i `c` \st -> (st, ss)
          ws s = w (s ++)

          i st@(State _ False)  = (st, nullSS)
          i    (State lvl True) = ((State lvl False), ((replicate (lvl * 2) ' ') ++))

          nl = \(State lvl _) -> (State lvl True, ("\n" ++))
          inc = \(State lvl b) -> (State (lvl+1) b, nullSS)
          dec = \(State lvl b) -> (State (lvl-1) b, nullSS)

          c :: PP -> PP -> PP
          c f1 f2 = \st -> let (st2, ss)  = f1 st
                               (st3, ss2) = f2 st2
                            in (st3, ss . ss2)
          nullSS = ("" ++)

mkdirRec = createDirectoryIfMissing True

suffixChop suffix filepath
    | suffix `isSuffixOf` filepath = dropLast (length suffix) filepath
    | otherwise                    = filepath
    where dropLast n = reverse . drop n . reverse

getDirsAndFiles dir = do
    allFiles <- getDirStat dir 
    return (map fst $ filter (isDirectory . snd) allFiles, map fst $ filter (isRegularFile . snd) allFiles)

getDirStat dir = getDir dir >>= mapM (\e -> liftM ((,) e) (getFileStatus e))
getDir dir = map (dir </>) . filter (not . flip elem [".",".."]) <$> getDirectoryContents dir

basenameNoExt = takeBaseName
basename = snd . splitFileName

splitOn p s = case dropWhile p s of
                   "" -> []
                   s' -> w : splitOn p s''
                         where (w, s'') = break p s'

whileM :: Monad m => m Bool -> m () -> m ()
whileM c f = do
    b <- c
    if b
        then f >> whileM c f
        else return ()

-- | map one element in a list if found
mapOne :: Eq a => a -> (a -> a) -> [a] -> [a]
mapOne _  _ []     = []
mapOne el f (x:xs)
    | x == el      = f x : xs
    | otherwise    = x : mapOne el f xs

-- | map one element in a list if found, otherwise append an element.
mapOneAppend :: Eq a => a -> (a -> a) -> a -> [a] -> [a]
mapOneAppend _  _ toAppend []     = [toAppend]
mapOneAppend el f toAppend (x:xs)
    | x == el                     = f x : xs
    | otherwise                   = x : mapOneAppend el f toAppend xs

writeFileSafely filename s = writeFile tmpFilename s >> rename tmpFilename filename
    where tmpFilename = filename ++ ".tmp"

-- | read the file or 
readFileSafely r filename = E.catch (readFile filename) (\(_ :: E.SomeException) -> return r)

newtype Uuid = Uuid String
    deriving (Eq)

instance Show Uuid where
    show (Uuid s) = s
