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

module UpdateMgr.MetaParse where

import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import UpdateMgr.Types
import Tools.Misc
import Tools.Text

colonDictFile :: String -> Map String String
colonDictFile =
    Map.fromList
           . mapMaybe (tuple . split ':')
           . lines
    where
      tuple (k:v:_) = Just (strip k,strip v)
      tuple _ = Nothing

data Repo = Repo { repoPack :: String
                 , repoProduct :: String
                 , repoBuild :: String
                 , repoVersion :: String -- user friendly version
                 , repoRelease :: String -- real version
                 , repoUpgradeFrom :: [String] }

parseRepo :: String -> Maybe Repo
parseRepo str = do
  let look k = Map.lookup k $ colonDictFile str
  pack <- look "pack"
  product <- look "product"
  build <- look "build"
  version <- look "version"
  release <- look "release"
  upgradefrom <- words <$> look "upgrade-from"
  return Repo { repoPack = pack
              , repoProduct = product
              , repoBuild = build
              , repoVersion = version
              , repoRelease = release
              , repoUpgradeFrom = upgradefrom }

descriptionOfRepo :: Repo -> String
descriptionOfRepo repo =
    repoProduct repo ++ " " ++ repoVersion repo

applicableOfRepo :: Repo -> [XcVersion]
applicableOfRepo repo = map XcVersion (repoUpgradeFrom repo)

parsePackages :: String -> [FileMeta]
parsePackages = mapMaybe parse_line . lines where
    parse_line = columns . words
    columns (a:b:c:d:e:f:g:_) = do
      len <- maybeRead b
      sha256 <- maybeRead ("0x"++c)
      return
             FileMeta { fileName = f
                      , fileRole = a
                      , fileType = d
                      , fileLength = len
                      , fileSHA256 = sha256 }
    columns _ = Nothing

parseMeta :: (String, String) -> Maybe Meta
parseMeta (repo_s, packages_s) = do
  repo <- parseRepo repo_s
  return
    Meta { metaApplicableTo = applicableOfRepo repo
         , metaDescription = descriptionOfRepo repo
         , metaProduct = repoProduct repo
         , metaVersion = XcVersion (repoRelease repo)
         , metaHumaneVersion = XcHumaneVersion (repoVersion repo)
         , metaFiles = parsePackages packages_s }
