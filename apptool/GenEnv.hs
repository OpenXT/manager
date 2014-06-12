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

module GenEnv
       (
         genVmEnvDoc
       , genVmEnvIso
       ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad
import qualified Control.Exception as E

import System.FilePath
import System.Directory
import System.IO

import Tools.Process
import Tools.Misc
import Tools.IfM

import Util
import Core.Types
import Core.Uuid
import Rpc.Core
import qualified OVF.Model as Ovf
import qualified OVF.Env as Ovf
import VirtualSystem
import Appliance
import AppInventory
import Vm.ProductProperty (vmPPList)

inform :: MonadIO m => String -> m ()
inform m = liftIO $ hPutStrLn stderr m

genVmEnvDoc :: MonadRpc e m => Uuid -> m String
genVmEnvDoc vm = go =<< findVmAppliances vm where
  go [ ] = error $ "genEnvDoc: vm " ++ show vm ++ " is not part of any appliance"
  go (app_id:_) = gen app_id
  gen app_id = do
    sibling_vms  <- filter (/= vm) <$> applianceOwnedVms app_id
    env <- mkVmEnv app_id vm
    sibling_envs <- mapM (mkVmEnv app_id) sibling_vms
    return . Ovf.envStr $ Ovf.env env sibling_envs
  
mkVmEnv :: MonadRpc e m => AppID -> Uuid -> m Ovf.VmEnv
mkVmEnv app vm = Ovf.VmEnv <$> (mkOvfID <$> getSystemID app vm) <*> vmPPList vm where
  mkOvfID (VirtualSystemID id) = Ovf.SystemID id

vmOvffilesPath vm = "/storage/ovffiles/" </> show vm

genVmEnvIso :: MonadRpc e m => FilePath -> FilePath -> Uuid -> m ()
genVmEnvIso tempRoot isoPath vm = do
  opuuid <- liftIO uuidGen
  doc <- genVmEnvDoc vm
  apps <- findVmAppliances vm
  liftIO $ do
    let folder = tempRoot </> "iso-" ++ show opuuid
    createDirectoryIfMissing True folder
    E.finally (assemble folder doc apps) (rmDir folder)
  where
    assemble root doc apps = do
      writeFile (root </> "ovf-env.xml") doc
      mapM_ cpEnvFiles apps
      mkisofs isoPath root
      where cpEnvFiles app = do
              whenM (doesDirectoryExist (vmOvffilesPath vm)) $ do
                inform $ "copy " ++ (vmOvffilesPath vm) ++ " -> " ++ root
                cpDirRec (vmOvffilesPath vm) root
  
rmDir folder = void$ readProcessOrDie "rm" ["-rf", folder] "" 
mkisofs isopath root = void$ readProcessOrDie "mkisofs" ["-r", "-J", "-o", isopath, root] ""
