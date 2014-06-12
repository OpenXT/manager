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

module UpdateMgr.UpdateMgrRpc where

import Control.Concurrent
import Data.String
import Data.Maybe
import Tools.Log
import Tools.IfM
import Rpc.Autogen.UpdatemgrServer
import Rpc.Core
import UpdateMgr.Rpc
import UpdateMgr.Types
import UpdateMgr.Logic
import UpdateMgr.UpdateMonad
import UpdateMgr.Error
import UpdateMgr.Background
import UpdateMgr.App
import Tools.Error

expose :: Job -> App ()
expose job = liftRpc . rpcExpose (fromString "/") . interfaces . implementation job =<< getAppState

-- symlink in /storage/update-staging

implementation :: Job -> AppState -> UpdatemgrServer Rpc
implementation job appstate = let onlySynchronizerAllowed :: Rpc Bool
                                  onlySynchronizerAllowed = return False
                                  -- for the moment, we always allow
                                  -- manual updates.  Eventually we
                                  -- want to give the option of
                                  -- restricting updates to whatever
                                  -- the synchronizer says.
                              in UpdatemgrServer {
      comCitrixXenclientUpdatemgrCheckUpdate =
        \url -> ifM onlySynchronizerAllowed (throwError $ localE Forbidden) $ do
          (mmeta, applicable, _) <- foregroundUpdate' appstate (finally cancelUpdate $ checkUpdate url)
          case mmeta of
              Nothing -> throwError $ localE UpdateNotApplicable
              Just meta -> let XcVersion v = metaVersion meta
                               XcHumaneVersion hv = metaHumaneVersion meta in
                           return (hv, v, updateApplicabilityStr applicable)

  ,    comCitrixXenclientUpdatemgrCheckUpdateLatest = \url -> ifM onlySynchronizerAllowed (throwError $ localE Forbidden) $ do
          (mmeta,applicable, _) <- foregroundUpdate' appstate . finally cancelUpdate $ checkUpdateLatest url
          case mmeta of
              Just meta -> let XcVersion v = metaVersion meta
                               XcHumaneVersion hv = metaHumaneVersion meta in
                           return (hv, v, updateApplicabilityStr applicable)
              Nothing -> return ("", "", updateApplicabilityStr applicable)

  , comCitrixXenclientUpdatemgrDownloadUpdate = \url -> ifM onlySynchronizerAllowed (throwError $ localE Forbidden) $ do
      foregroundUpdate' appstate (downloadAndCheckUpdateMeta url)
      backgroundUpdate' appstate job (savedFailure downloadFiles)

  , comCitrixXenclientUpdatemgrDownloadUpdateLatest = \url -> ifM onlySynchronizerAllowed (throwError $ localE Forbidden) $ do
      foregroundUpdate' appstate (downloadAndCheckUpdateMetaLatest url)
      backgroundUpdate' appstate job (savedFailure downloadFiles)

  , comCitrixXenclientUpdatemgrApplyUpdateAndReboot = ifM onlySynchronizerAllowed (throwError $ localE Forbidden) $ do
      -- run some checks
      foregroundUpdate' appstate applyUpdateAssertions
      -- actually apply update (in background)
      backgroundUpdate' appstate job (savedFailure applyUpdateAndReboot)

  , comCitrixXenclientUpdatemgrApplyUpdateAndShutdown = ifM onlySynchronizerAllowed (throwError $ localE Forbidden) $ do
      -- run some checks
      foregroundUpdate' appstate applyUpdateAssertions
      -- actually apply update (in background)
      backgroundUpdate' appstate job (savedFailure applyUpdateAndShutdown)

  , comCitrixXenclientUpdatemgrCancelUpdate = ifM onlySynchronizerAllowed (throwError $ localE Forbidden) $ liftIO (cancelJob job) >> foregroundUpdate' appstate cancelUpdate
  , comCitrixXenclientUpdatemgrGetUpdateApplicable = ifM onlySynchronizerAllowed (throwError $ localE Forbidden) $ updateApplicabilityStr <$> foregroundUpdate' appstate getUpdateApplicability
  , comCitrixXenclientUpdatemgrGetUpdateUrl = foregroundUpdate' appstate getUpdateURL
  , comCitrixXenclientUpdatemgrGetUpdateState = foregroundUpdate' appstate (updateStateStr <$> getUpdateState)
  , comCitrixXenclientUpdatemgrGetUpdateFailReason = foregroundUpdate' appstate getUpdateFailReason
  , comCitrixXenclientUpdatemgrGetUpdateDescription = foregroundUpdate' appstate getUpdateDescription
  , comCitrixXenclientUpdatemgrGetUpdateDownloadPercent = realToFrac <$> foregroundUpdate' appstate getUpdateDownloadPercent
  , comCitrixXenclientUpdatemgrGetUpdateDownloadSpeed = realToFrac <$> foregroundUpdate' appstate getCurrentDownloadSpeed
}
