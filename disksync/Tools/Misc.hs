--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

module Tools.Misc (
               split
             , strip
             , chomp
             , spawnShell
             , spawnShell'
             , safeSpawnShell
             , differenceList
             , getCurrentRunLevel
             , fileSha1Sum
             , replace
             , endsWith
             , uuidGen
             ) where

import qualified Data.Text as T
import Data.String
import Control.Applicative
import System.Process
import System.IO
import System.Exit
import List ( isPrefixOf )
import Tools.File

-- chop line ending characters off
chomp :: String -> String
chomp s = case reverse s of
            ('\n' : '\r' : xs) -> reverse xs
            ('\n' : xs)        -> reverse xs
            _                  -> s

-- Split a list over an element
split :: (Eq a) => a -> [a] -> [[a]]
split sep xs =
    let (y,ys) = span (/= sep) xs in
    case ys of
      [] -> [y]
      zs -> y : split sep (tail zs)


-- Strip a string from whitespace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- Execute shell command and wait for its output, return empty string in case of exit failure
spawnShell :: String -> IO String
spawnShell cmd =
    spawnShell' cmd >>= f where f Nothing  = return ""
                                f (Just s) = return s

-- Execute shell command and wait for its output, return Nothing on failure exit code
spawnShell' :: String -> IO (Maybe String)
spawnShell' cmd =
    runInteractiveCommand cmd >>= \ (_, stdout, _, h) ->
        do contents <- hGetContents stdout
           -- force evaluation of contents
           exitCode <- length contents `seq` waitForProcess h
           case exitCode of
             ExitSuccess -> return $ Just contents
             _           -> return   Nothing

-- Execute shell command and wait for its output, cause exception on failure exit code
safeSpawnShell :: String -> IO String
safeSpawnShell cmd =
    spawnShell' cmd >>= f where
        f Nothing  = error $ message
        f (Just s) = return s
        message    = "shell command: " ++ cmd ++ " FAILED."

differenceList :: (Eq a) => [a] -> [a] -> [a]
differenceList xs ys = [x | x <- xs, not (x `elem` ys)]

getCurrentRunLevel :: IO Int
getCurrentRunLevel = do
  Just runlevelStr <- spawnShell' "runlevel"
  let [prevStr, currentStr] = words runlevelStr
  return $ read currentStr

fileSha1Sum :: FilePath -> IO Integer
fileSha1Sum path = do
    Just (sumStr:_) <- fmap (reverse . words) <$> (spawnShell' $ "openssl dgst -sha1 \"" ++ path ++ "\"")
    return $ read ("0x" ++ sumStr)

replace :: String -> String -> String -> String
replace pat repl txt =
    T.unpack $ T.replace (T.pack pat) (T.pack repl) (T.pack txt)

endsWith :: (Eq a) => [a] -> [a] -> Bool
endsWith suffix s = (reverse suffix) `isPrefixOf` (reverse s)

uuidGen :: IsString a => IO a
uuidGen =
    readFile "/proc/sys/kernel/random/uuid" >>= return . fromString . strip
  where strip = T.unpack . T.strip . T.pack

