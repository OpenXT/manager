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

{-# LANGUAGE ScopedTypeVariables #-}
module StoreHouse.VHD.Forest
	( VhdInfo(..)
	, vhdGetInfo
	, VhdChain(..)
	, vhdGetChain
	, vhdsGraph
	) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Either
import Data.Word
import qualified Data.Vhd as Vhd

import Control.Applicative
import Control.Monad
import Control.Exception

import System.IO
import System.Process
import System.Directory
import System.Exit
import System.FilePath

import Tools.Uuid
import Tools.Log

data VhdInfo = VhdInfo
    { vhdUuid   :: Uuid KindVHD
    , vhdPath   :: FilePath
    , vhdParent :: Maybe (Uuid KindVHD, FilePath)
    } deriving (Show,Eq)

data VhdChain =
      ChainWithParent FilePath (Uuid KindVHD) VhdChain
    | ChainEnd FilePath (Uuid KindVHD)
    | ChainError FilePath String
    deriving (Show,Eq)

vhdGetInfo :: FilePath -> FilePath -> IO (Either String VhdInfo)
vhdGetInfo dir filepath = do
	e <- try $ Vhd.getInfo (dir </> filepath)
	case e of
		Left (exn :: SomeException)    -> return $ Left $ show exn
		Right (Left err)               -> return $ Left err
		Right (Right (header, footer)) -> do
			let (Vhd.ParentUnicodeName parentFilepath) = Vhd.headerParentUnicodeName header
			return $ Right $ VhdInfo
				{ vhdUuid   = uuidFromString $ show $ Vhd.footerUniqueId footer
				, vhdPath   = filepath
				, vhdParent = if parentFilepath == ""
					then Nothing
					else Just (uuidFromString $ show $ Vhd.headerParentUniqueId header, parentFilepath)
				}

vhdGetChain :: FilePath -> FilePath -> IO VhdChain
vhdGetChain dir filepath = do
	minfo <- vhdGetInfo dir filepath
	case minfo of
		Left err      -> return $ ChainError filepath err
		Right vhdinfo -> case vhdParent vhdinfo of
			Nothing                 -> return $ ChainEnd filepath (vhdUuid vhdinfo)
			Just (puuid, pfilepath) -> do
				parentChain <- vhdGetChain dir pfilepath
				return $ ChainWithParent filepath (vhdUuid vhdinfo) parentChain

-- | return all the tree leaves of a vhd forest.
vhdsGraph :: FilePath -> [FilePath] -> IO [VhdChain]
vhdsGraph dir paths = do
	infos <- rights <$> mapM (vhdGetInfo dir) paths
	mapM (vhdGetChain dir . vhdPath) (loop infos infos) -- inefficient
	where
		loop infos []     = []
		loop infos (x:xs) = if null $ filter (vhdMatched (vhdUuid x)) infos
			then x : loop infos xs
			else loop infos xs

		vhdMatched uuid vhdinfo = case vhdParent vhdinfo of
			Nothing         -> False
			Just (puuid, _) -> puuid == uuid
