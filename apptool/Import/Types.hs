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

module Import.Types
       (
         ImportError (..)
       , ImportOptions (..)
       , ImportMode (..)
       , module Core.Types
       ) where

import System.FilePath
import Rpc.Core
import Core.Types

data ImportError
   = ImportRpcError RpcCall RemoteErr
   | ImportIOError String
   | VmAlreadyExists Uuid
   | InternalImportError String
   | FileDoesNotExist FilePath
   | UnsupportedFileURL String
   | FilesystemImageFileNotSpecified String
   | CPIOImageFileNotSpecified String
   | InvalidEncryptionKeySize FilePath
   | EncryptionKeyAlreadyExists FilePath
   | VmInstallTimeout String Int
     deriving (Show)

instance IsRemoteError ImportError where
    fromRemoteErr = ImportRpcError
    toRemoteErr (ImportRpcError _ dbusE) = Just dbusE
    toRemoteErr _  = Nothing

data ImportMode
   = ImportImagesOnly { imagesMapFile :: FilePath }
   | ImportWithExistingImages { imagesMapFile :: FilePath }
   | ImportAll
     
data ImportOptions
   = ImportOptions {
         importDiskFolder :: FilePath
       , importIsoFolder :: FilePath
       , importDomStoreFolder :: FilePath
       , importCryptoKeysFolder :: FilePath
       , importOvfFilesFolder :: FilePath
       , importMode :: ImportMode
       , allowUpgrades :: Bool
       , allowDowngrades :: Bool
       , allowReinstall :: Bool
     }
