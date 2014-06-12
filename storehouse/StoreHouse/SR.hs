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

{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module StoreHouse.SR
    ( SRable(..)
    , PhysSR(..)
    , physSRNew
    , SR(..)
    , VdiInfo(..)
    , srGetTotalSize
    , srGetFreeSize
    , srCreateVdi
    , srCreateVdiWith
    , srDeleteVdi
    , srListVdis
    , srListNodes
    , srSnapshotVdi
    , srVdiListNodes
    , srVdiPath
    , srGetVdiInfo
    , srSetKey
    , srGetKey
    , srDelKey
    , srStreamDownload
    , srStreamUpload
    , srStreamClose
    ) where

import Tools.Uuid
import Tools.Log
import Tools.File
import Data.Word
import Data.Int
import Data.Maybe (catMaybes)
import StoreHouse.VHD.Util
import StoreHouse.VHD.Forest
import StoreHouse.VHD.Graph
import StoreHouse.Vdi
import System.FilePath
import System.Posix.Files
import System.Directory
import qualified Data.ByteString as B

import Control.Exception
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans

data VdiType =
	  VdiTypeVhd
	| VdiTypeFile
	| VdiTypeDev
	deriving (Eq)

instance Show VdiType where
	show VdiTypeVhd  = "vhd"
	show VdiTypeFile = "file"
	show VdiTypeDev  = "raw"

data VdiInfo = VdiInfo
	{ vdiInfoPhysicalSize :: Word64
	, vdiInfoPhysicalPath :: FilePath
	, vdiInfoType         :: VdiType
	, vdiInfoHash         :: Integer -- use a hash type
	} deriving (Show,Eq)

type NodeID = String
type KeyData = B.ByteString

class SRable a where
    srableGetTotalSize :: a -> IO Word64
    srableGetFreeSize  :: a -> IO Word64
    srableCreateVdi    :: a -> Word64 -> IO (Uuid KindVDI)
    srableCreateVdiWith :: a -> NodeID -> IO (Uuid KindVDI)
    srableDeleteVdi    :: a -> Uuid KindVDI -> Bool -> IO ()
    srableSnapshotVdi  :: a -> Uuid KindVDI -> IO ()
    srableListVdis     :: a -> IO [Uuid KindVDI]
    srableListNodes    :: a -> IO [NodeID]
    srableVdiListNodes :: a -> Uuid KindVDI -> IO [NodeID]
    srableGetVdiInfo   :: a -> Uuid KindVDI -> IO (Maybe VdiInfo)
    srableVdiPath      :: a -> Uuid KindVDI -> IO (Maybe FilePath)
    -- key management
    srableSetKey       :: a -> NodeID -> KeyData -> IO ()
    srableGetKey       :: a -> NodeID -> IO KeyData
    srableDelKey       :: a -> NodeID -> IO ()

data SR = forall s . SRable s => SR s

srGetTotalSize (SR z)  = srableGetTotalSize z
srGetFreeSize  (SR z)  = srableGetFreeSize  z
srCreateVdi    (SR z)  = srableCreateVdi    z
srCreateVdiWith (SR z) = srableCreateVdiWith z
srDeleteVdi    (SR z)  = srableDeleteVdi    z
srListVdis     (SR z)  = srableListVdis     z
srListNodes    (SR z)  = srableListNodes    z
srGetVdiInfo   (SR z)  = srableGetVdiInfo   z
srSnapshotVdi  (SR z)  = srableSnapshotVdi  z
srVdiListNodes (SR z)  = srableVdiListNodes z
srVdiPath      (SR z)  = srableVdiPath      z
srSetKey       (SR z)  = srableSetKey       z
srGetKey       (SR z)  = srableGetKey       z
srDelKey       (SR z)  = srableDelKey       z

data PhysSR = PhysSR
    { physGetPath  :: FilePath
    , physGraph    :: VHDGraph
    }

instance SRable PhysSR where
    srableGetTotalSize physSR    = fst <$> diskSpaceInfo (physGetPath physSR)
    srableGetFreeSize  physSR    = snd <$> diskSpaceInfo (physGetPath physSR)
    srableListVdis     physSR    = vdiList (physGetPath physSR)
    srableListNodes    physSR    = getDirectoryContents_extension (physGetPath physSR) ".vhd"
    srableCreateVdi    physSR sz = do
        vdiUuid <- uuidGen
        vhdname <- (++ ".vhd") . show <$> uuidGen -- merely to create a random name, the actual uuid is generated during the vhd create operation.
        let vhdpath = (physGetPath physSR) </> vhdname
        createDirectoryIfMissing True (physGetPath physSR)
        vhdCreate vhdpath sz
        vdiFileWrite (physGetPath physSR) vdiUuid vhdname
        return vdiUuid

    srableCreateVdiWith physSR node = do
        assertNodeName node
        vdiUuid <- uuidGen
        let vhdname = show node ++ ".vhd"
        assertFileExists vhdname
        let vhdpath = (physGetPath physSR) </> vhdname
        vdiFileWrite (physGetPath physSR) vdiUuid vhdname
        return vdiUuid

    srableDeleteVdi physSR vdi delNodes = do
        exist <- vdiFileExist (physGetPath physSR) vdi
        when exist $ do
            when delNodes $ do
                vdihead <- maybe (error "cannot process VDI") id <$> vdiFileRead (physGetPath physSR) vdi
                chain   <- vhdGetChain (physGetPath physSR) vdihead
                -- check exists before removing in case error was file not found
                -- won't cope with race conditions but none of storehouse does
                -- (do we serialize dbus calls in to get round this?)
                let remove p = do info $ "removing " ++ p
                                  exist <- doesFileExist p
                                  when exist $ removeFile p
                -- remove files starting from root
                let removeVhds (ChainWithParent f _ p) = do removeVhds p
                                                            remove f
                    removeVhds (ChainEnd f _)          = remove f
                    -- ignore error in chain
                    removeVhds (ChainError f _)        = remove f
                removeVhds chain
            vdiFileDelete (physGetPath physSR) vdi

    srableSnapshotVdi physSR vdi = do
        exist <- vdiFileExist (physGetPath physSR) vdi
        when exist $ do
            oldname <- maybe (error "cannot process VDI") id <$> vdiFileRead (physGetPath physSR) vdi
            newname <- (++ ".vhd") . show <$> uuidGen
            vhdSnapshot (physGetPath physSR </> oldname) (physGetPath physSR </> newname)
            vdiFileWrite (physGetPath physSR) vdi newname

    srableVdiListNodes physSR vdi = do
        exist <- vdiFileExist (physGetPath physSR) vdi
        if exist
            then do
                vdihead <- maybe (error "cannot process VDI") id <$> vdiFileRead (physGetPath physSR) vdi
                chain   <- vhdGetChain (physGetPath physSR) vdihead
                return $ chainToName chain
			else return []
        where
            chainToName (ChainWithParent n _ parent) = n : chainToName parent
            chainToName (ChainEnd n _)    = [n]
            chainToName (ChainError n e) = error ("not a valid chain: " ++ e ++ " at: " ++ n)

    srableGetVdiInfo physSR uuid = either (const Nothing) Just <$> getInfo physSR uuid

    srableVdiPath physSR vdi = do
        exist <- vdiFileExist (physGetPath physSR) vdi
        if exist
            then fmap (physGetPath physSR </>) <$> vhdHeadOfVdi (physGetPath physSR) vdi
            else return Nothing

    srableGetKey physSR node = do
        assertNodeName node
        return B.empty
    srableSetKey physSR node _ = do
        assertNodeName node
    srableDelKey physSR node = do
        assertNodeName node

physSRNew dir = do
    vdis <- vdiList dir
    paths <- catMaybes <$> mapM (vdiFileRead dir) vdis
    graph <- initWithPaths dir paths
    return $ SR $ PhysSR dir graph

vhdHeadOfVdi :: FilePath -> Uuid KindVDI -> IO (Maybe FilePath)
vhdHeadOfVdi path vdi = fmap (path </>) <$> vdiFileRead path vdi

getInfo :: PhysSR -> Uuid KindVDI -> IO (Either SomeException VdiInfo)
getInfo physSR uuid = try $ do
	filepath <- maybe (error "no vdi present") id <$> vhdHeadOfVdi (physGetPath physSR) uuid
	fstat    <- getFileStatus filepath
	return $ VdiInfo
		{ vdiInfoPhysicalSize = fromIntegral $ fileSize fstat
		, vdiInfoPhysicalPath = filepath
		, vdiInfoType         = VdiTypeVhd
		, vdiInfoHash         = 0
		}

diskSpaceInfo :: String -> IO (Word64, Word64)
diskSpaceInfo path = do
	size  <- fromIntegral <$> getBlockSize path
	total <- getTotalBlocks path
	avail <- getAvailBlocks path
	return (fromIntegral ((total*size) `div` mib), fromIntegral ((avail*size) `div` mib))
	where mib = 1024 * 1024


srStreamUpload :: SR -> IO String
srStreamUpload (SR z) = do
	undefined

srStreamDownload :: SR -> IO String
srStreamDownload (SR z) = do
	undefined

srStreamClose :: SR -> String -> IO ()
srStreamClose (SR z) = do
	undefined

assertNodeName nodeName = when (and $ map (not . (== '/')) nodeName) errorOut
    where errorOut = error ("node name is not valid")

assertFileExists file = doesFileExist file >>= flip when errorOut
    where errorOut = error ("file " ++ show file ++ " doesn't exists")
