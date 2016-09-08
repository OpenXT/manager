--
-- Copyright (c) 2014 Citrix Systems, Inc.
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
module UpdateMgr.Logic where

import Data.Map (Map)
import Data.Char
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import System.FilePath
import Text.Printf

import Control.Monad.Loops
import UpdateMgr.Types
import UpdateMgr.UpdateMonad
import UpdateMgr.Error
import UpdateMgr.MetaParse
import UpdateMgr.Curl (DownloadInitErr(..))
import Tools.IfM
import Tools.Misc
import Tools.Error
import Tools.Text

updateDirBase :: FilePath
updateDirBase = "/storage/update"

updateStagingSymLink :: FilePath
updateStagingSymLink = "/storage/update-staging"

updateDirUntarTemp :: FilePath
updateDirUntarTemp = updateDirBase </> "untarTemp"

updateDirUntarTempP = updateDirUntarTemp </> "packages.main"

updateDirCurrent :: FilePath
updateDirCurrent = updateDirBase </> "current"

releasesInfoPath :: FilePath
releasesInfoPath = updateDirBase </> "releases"

xenclientConf :: FilePath
xenclientConf = "/etc/xenclient.conf"

repoCertConf :: FilePath
repoCertConf = "/config/repo-cert.conf"

packagesMetaURL :: URL -> URL
packagesMetaURL repository = repository ++ "/packages.main/XC-PACKAGES"

repoMetaURL :: URL -> URL
repoMetaURL repository = repository ++ "/packages.main/XC-REPOSITORY"

signatureMetaURL :: URL -> URL
signatureMetaURL repository = repository ++ "/packages.main/XC-SIGNATURE"

updateStatePath :: Path
updateStatePath = "/updatemgr/update-state"

failReasonPath :: Path
failReasonPath = "/updatemgr/update-fail-reason"

updateURLPath :: Path
updateURLPath = "/updatemgr/update-url"

updateURLReleasesInfoPath :: Path
updateURLReleasesInfoPath = "/updatemgr/update-repository-url"

instance DbRepr UpdateState where
    toDbTree state = Leaf (updateStateStr state)
    fromDbTree (Leaf str) = updateStateFromStr str
    fromDbTree _ = Nothing

-- TODO: The fragment (mapMaybe (safeHead2 . split '=') . lines)
-- should go into its own library, because it occurs so often in other
-- files, too.

dictFile :: String -> Map String String
dictFile =
    Map.fromList
           . mapMaybe (safeHead2 . split '=')
           . lines
    where
      safeHead2 (k:v:_) = Just (strip k,strip v)
      safeHead2 _ = Nothing

currentXcVersion :: Update XcVersion
currentXcVersion =
    get . dictFile <$> fileContents xenclientConf where
        get m = let Just v = Map.lookup "release" m in (XcVersion v)

allowDevRepoCert :: Update Bool
allowDevRepoCert =
    ifM (fileExists repoCertConf)
        (get . dictFile <$> fileContents repoCertConf)
        (return False)
    where
      get (Map.lookup "ALLOW_DEV_REPO_CERT" -> Just "'true'") = True
      get _ = False

-- Will resume if file is already there,
-- finishes when download finishes, otherwise (in case of error or cancellation) terminates with exception
downloadTo :: Bool -> URL -> FilePath -> Update ()
downloadTo track url path = do
  logInfo $ "downloading " ++ show url ++ " to " ++ show path
  handleOrError <- ifM (fileExists path)
    (resumeDownload url path)
    (beginDownload url path)
  case handleOrError of
    Left ex -> case ex of
                 (CurlErr 6 _)      -> throwError . localE $ CannotResolveHost url
                 (CurlErr 7 _)      -> throwError . localE $ CannotConnectHost url
                 (CurlErr code msg) -> throwError . localE $ DownloadFailed url
                 HttpHeaderErr      -> throwError . localE $ DownloadFailed url
                 HttpErr (4,0,4)    -> throwError . localE $ FileNotFound url
                 HttpErr (a,b,c)    -> throwError . localE $ DownloadFailed url
    Right handle -> do
           setCurrentDownload (Just handle)
           events handle
  where
    events h = getNextDownloadEvent h >>= \e -> logInfo (show e) >> event e
        where
          event DownloadFinished = setCurrentDownload Nothing
          event DownloadCancelled = setCurrentDownload Nothing >> (throwError . localE $ DownloadWasCancelled)
          event DownloadError = setCurrentDownload Nothing >> (throwError . localE $ DownloadFailed url)
          event (DownloadProgressed p) = case track of
              True -> (do p' <- total_progress p
                          setCurrentDownloadSpeed (dlSpeed p')
                          notifyUpdateDownloadProgress p'
                          events h)
              False -> (events h)
    -- TODO: optimise me to not reread headers
    total_progress p = DownloadProgress <$>
      (calcDownloadPercent =<< getDownloadFiles) <*>
      return (dlSpeed p)

downloadFile :: FileDownload -> Update ()
downloadFile f = unlessM (anyM download [1..tries]) $ abort

  where download i = do downloadTo True (downloadURL f) (downloadPath f)
                        ifM (verifyDownloadedFile f)
                            (do logInfo ( "Verification of " ++ downloadPath f ++ " OK" ++ attempt)
                                return True)
                            (do logInfo $ "Verification of " ++ downloadPath f ++ " FAILED" ++ attempt ++
                                    " Retrying download of " ++ show (downloadURL f)
                                rmFile (downloadPath f)
                                return False)
            where attempt = " in attempt " ++ show i ++ " of " ++ show tries ++ "."
        abort    = do logInfo $ "Verification of Download of " ++ show (downloadURL f) ++ " FAILED " ++ show tries ++ " times.  Aborting."
                      throwError . localE $ CorruptFile (downloadPath f)
        -- Ticket XC-6903 suggested 4 tries before giving up.  That's as good as any other number.
        tries = 4

verifyDownloadedFile :: FileDownload -> Update Bool
verifyDownloadedFile f = do
    actual_len <- fileLen (downloadPath f)
    actual_chk <- computeSHA256 (downloadPath f)
    logInfo $ printf "SHA256 sum of %s = %X" (downloadPath f) actual_chk
    case () of
      _ | actual_len /= downloadLength f -> do
                           logInfo $ "file " ++ show (downloadPath f) ++ " length mismatch after finished download!"
                           return False
      _ | actual_chk /= downloadSHA256 f -> do
                           logInfo $ "file " ++ show (downloadPath f) ++ " SHA256 sum mismatch after finished download!"
                           return False
      _ -> return True

xcRepoMetaPath = updateDirCurrent </> "XC-REPOSITORY"
xcPackagesMetaPath = updateDirCurrent </> "XC-PACKAGES"
xcSignatureMetaPath = updateDirCurrent </> "XC-SIGNATURE"

-- TODO: Consider more elaborate syntax with comments,
--       or going even simpler, and not even filtering out blank lines.
downloadReleasesFile :: URL -> Update [URL]
downloadReleasesFile url = do
    downloadTo False url releasesInfoPath
    filter (not . all isSpace) . lines <$> fileContents releasesInfoPath

downloadMetaData :: URL -> Update Meta
downloadMetaData url = do
  setUpdateState DownloadingMeta
  logInfo "downloading repository description"
  downloadTo False (repoMetaURL url) xcRepoMetaPath
  logInfo "downloading packages description"
  downloadTo False (packagesMetaURL url) xcPackagesMetaPath
  logInfo "downloading signature description"
  downloadTo False (signatureMetaURL url) xcSignatureMetaPath
  currentUpdateMeta

haveMeta :: Update Bool
haveMeta = and <$> mapM fileExists [xcRepoMetaPath, xcPackagesMetaPath, xcSignatureMetaPath]

currentUpdateMeta :: Update Meta
currentUpdateMeta = do
  unlessM haveMeta (throwError $ localE MissingMetadata)
  repo_meta <- fileContents xcRepoMetaPath
  packages_meta <- fileContents xcPackagesMetaPath
  case parseMeta (repo_meta, packages_meta) of
    Nothing -> throwError $ localE MalformedMetadata
    Just m  -> return m

getUpdateState :: Update UpdateState
getUpdateState = dbReadWithDefault NoUpdate updateStatePath

getUpdateURL :: Update URL
getUpdateURL = dbReadWithDefault "" updateURLPath

getUpdateReleasesInfoURL :: Update URL
getUpdateReleasesInfoURL = dbReadWithDefault "" releasesInfoPath

setUpdateState :: UpdateState -> Update ()
setUpdateState s = go s >> notifyUpdateStateChange s where
    go NoUpdate = dbRm updateStatePath
    go s        = dbWrite updateStatePath s

getUpdateFailReason :: Update String
getUpdateFailReason = fromMaybe "" <$> dbMaybeRead failReasonPath

checkApplicability :: Meta -> Update UpdateApplicability
checkApplicability update_meta = do
  current <- currentXcVersion
  let server = metaVersion update_meta
  case () of
    _ | current == server -> return UpToDate
      | current `elem` metaApplicableTo update_meta -> return CanUpgrade
      | otherwise -> return CannotUpgrade

getUpdateApplicability :: Update UpdateApplicability
getUpdateApplicability =
  ifM haveMeta
    (checkApplicability =<< currentUpdateMeta)
    (return CannotUpgrade)

getUpdateDescription :: Update String
getUpdateDescription =
  ifM haveMeta
    (metaDescription <$> currentUpdateMeta)
    (return "no update in progress")

checkUpdateLatest :: URL -> Update (Maybe Meta, UpdateApplicability, URL)
checkUpdateLatest releasesUrl = do
    assertUpdateState [NoUpdate]
    -- erase update dir, download metadata, check applicability
    newEmptyDirectory updateDirBase
    urls <- downloadReleasesFile releasesUrl
    when (null urls) $
        throwError . localE $ NoRepositories releasesUrl
    logInfo "downloading metadata"

    -- Prefer last repository mentioned.
    foldr op last (reverse urls)

  where op url rest = do
            newEmptyDirectory updateDirCurrent
            meta <- downloadMetaData url
            logInfo "checking update applicability"
            applicable <- checkApplicability meta
            case applicable of
                CannotUpgrade -> rest
                _ -> do
                    setUpdateState DownloadedMeta
                    logInfo "verifying update metadata signatures"
                    verifyUpdateMetadataSignature
                    return (Just meta,applicable, url)
        last = return (Nothing, CannotUpgrade, "")

checkUpdate :: URL -> Update (Maybe Meta, UpdateApplicability, URL)
checkUpdate url = do
  assertUpdateState [NoUpdate]
  -- erase update dir, download metadata, check applicability
  newEmptyDirectory updateDirCurrent
  logInfo "downloading metadata"
  meta <- downloadMetaData url
  setUpdateState DownloadedMeta
  logInfo "checking update applicability"
  applicable <- checkApplicability meta
  logInfo "verifying update metadata signatures"
  verifyUpdateMetadataSignature
  return (Just meta,applicable, url)

downloadUpdateAssertions :: Update ()
downloadUpdateAssertions = do
  assertUpdatePolicy
  assertUpdateState [NoUpdate, DownloadedMeta]

downloadAndCheckUpdateMeta :: URL  -> Update ()
downloadAndCheckUpdateMeta = downloadAndCheckUpdateMetaAny checkUpdate

downloadAndCheckUpdateMetaLatest :: URL -> Update ()
downloadAndCheckUpdateMetaLatest = downloadAndCheckUpdateMetaAny checkUpdateLatest

downloadAndCheckUpdateMetaAny :: (URL -> Update (Maybe Meta, UpdateApplicability, URL)) -> URL -> Update ()
downloadAndCheckUpdateMetaAny how infoUrl = do
  downloadUpdateAssertions
  -- erase update dir, download & check metadata, download files
  newEmptyDirectory updateDirCurrent

  dbWrite releasesInfoPath infoUrl
  (meta,applicable, url) <- how infoUrl
  dbWrite updateURLPath url

  case applicable of
    UpToDate -> return ()
    CannotUpgrade -> throwError $ localE UpdateNotApplicable
    CanUpgrade -> return ()

downloadFiles :: Update ()
downloadFiles = do
  -- TODO: maybe when we fix curl fork
  -- waitNetworkAvailable
  mapM_ downloadFile =<< (getDownloadFiles <* setUpdateState DownloadingFiles)
  logInfo "finished download of all update files"
  setUpdateState DownloadedFiles

verifyUpdateMetadataSignature :: Update ()
verifyUpdateMetadataSignature = void $
  handleError failed . safeShellExecuteAndLogOutput . cmd =<< allowDevRepoCert
  where
    cmd False = "verify-repo-metadata " ++ updateDirCurrent
    cmd True  = "verify-repo-metadata -d " ++ updateDirCurrent
    failed _  = throwError $ localE FailedSignatureVerification

handleError = flip catchError

applyUpdateAssertions :: Update ()
applyUpdateAssertions = do
  assertUpdatePolicy
  assertUpdateState [DownloadedFiles]
  assertUpdateApplicable
  -- assertNoGuestVms

applyUpdateFromTarball :: FilePath -> Update ()
applyUpdateFromTarball filepath = void $ do
    (applyUpdateFromTarballAndReboot filepath) `catchError` onErr

    where
        onErr ex = do
          logInfo $ printf "Failed to apply update from tarball - %s" (show ex)
          dbWrite failReasonPath (show ex) 
          setUpdateState Failed
          rmFile filepath


-- Expects absolute path to tarball.
applyUpdateFromTarballAndReboot :: FilePath -> Update ()
applyUpdateFromTarballAndReboot tarball = do
    -- TODO: Be more graceful in the face of running updates.
    cancelUpdate
    -- ToDo: Think harder about the permissible UpdateStates.
    logInfo $ "in applyUpdateFromTarball"
    state <- getUpdateState
    logInfo $ "in stage: " ++ show state
    assertUpdateState [NoUpdate]
    logInfo $ "and in the right stage"
    setUpdateState Applying

    newEmptyDirectory updateDirUntarTemp
    unTarGZ tarball updateDirUntarTemp
    logInfo $ "unpacked tarball to updateDirUntarTemp."

    -- TODO: clean up constants and literals
    rmDirectory updateDirCurrent
    mv updateDirUntarTempP updateDirCurrent
    -- TODO: Check uncorruption, i.e. sha256.

    assertUpdateApplicable
    verifyUpdateMetadataSignature
    -- TODO: Consider reading from meta-data, and not just relying on the name.
    unTarGZ (updateDirCurrent </> "control.tar.bz2") updateDirCurrent
    logInfo "executing update script"
    safeShellExecuteAndLogOutput $ (updateDirCurrent </> "upgrade") ++ " -c " ++ updateDirCurrent ++ " " ++ updateDirCurrent
    rmFile tarball
    logInfo "update finished"
    setUpdateState Done
    rebootHost

applyUpdate :: Update ()
applyUpdate = do
  logInfo "request to start update operation"
  applyUpdateAssertions
  setUpdateState Applying
  logInfo "checking for file corruption"
  assertUncorruptFiles
  logInfo "verifying update metadata signatures"
  verifyUpdateMetadataSignature
  logInfo "unpacking control files"
  unpackControlFiles
  logInfo "executing update script"
  safeShellExecuteAndLogOutput $ (updateDirCurrent </> "upgrade") ++ " -c " ++ updateDirCurrent ++ " " ++ updateDirCurrent
  logInfo "update finished"
  setUpdateState Done

applyUpdateAndShutdown :: Update ()
applyUpdateAndShutdown = applyUpdate >> shutdownHost

applyUpdateAndReboot :: Update ()
applyUpdateAndReboot = applyUpdate >> rebootHost

-- continue update from some current state
continueUpdate :: Update ()
continueUpdate = fromState =<< getUpdateState where
    fromState NoUpdate = return ()
    fromState DownloadingReleasesInfo = cancelUpdate
    fromState DownloadedReleasesInfo = cancelUpdate
    fromState DownloadingMeta = cancelUpdate
    fromState DownloadedMeta = cancelUpdate
    fromState DownloadingFiles = cancelUpdate
    fromState DownloadedFiles = return ()
    fromState Applying = cancelUpdate
    fromState Failed = cancelUpdate
    fromState Done = cancelUpdate

cancelDownloads :: Update ()
cancelDownloads = fromD =<< getCurrentDownload where
    fromD Nothing  = return ()
    fromD (Just d) = cancelDownload d >> setCurrentDownload Nothing

cancelUpdate :: Update ()
cancelUpdate = fromState =<< getUpdateState where
    fromState NoUpdate = return ()
    fromState _ = do
      cancelDownloads
      rmDirectory updateDirBase
      dbRm failReasonPath
      setUpdateState NoUpdate

calcDownloadPercent :: [FileDownload] -> Update Float
calcDownloadPercent ds = (\done -> (if total_bytes > 0
                   then fromIntegral done / fromIntegral total_bytes
                   else 0)) <$> done_bytes where

    total_bytes  = sum $ map downloadLength ds
    done_bytes   = sum <$> mapM file_bytes ds
    file_bytes (downloadPath -> f) =
        ifM (fileExists f)
            (fileLen f)
            (return 0)
getDownloadFiles :: Update [FileDownload]
getDownloadFiles = do
  maybe_url <- dbMaybeRead updateURLPath
  case maybe_url of
      Nothing -> throwError . localE $ InternalError "database missing update URL"
      Just url -> map download_descr . metaFiles <$> currentUpdateMeta
        where
          download_descr filemeta =
              FileDownload { downloadURL = url / "packages.main" / fileName filemeta
                           , downloadPath = updateDirCurrent </> fileName filemeta
                           , downloadLength = fileLength filemeta
                           , downloadFileRole = fileRoleFromString (fileRole filemeta)
                           , downloadSHA256 = fileSHA256 filemeta }
            where (/) = concatURL

getControlFiles :: Update [FilePath]
getControlFiles = map downloadPath . filter (\f -> downloadFileRole f == ControlFile) <$> getDownloadFiles

getUpdateDownloadPercent :: Update Float
getUpdateDownloadPercent = from_state =<< getUpdateState where
    from_state DownloadingFiles = getDownloadFiles >>= calcDownloadPercent
    from_state _ = return 0

unpackControlFiles :: Update ()
unpackControlFiles = do
  assertUpdateState [Applying]
  mapM_ unpackControlFile =<< getControlFiles

unpackControlFile :: FilePath -> Update ()
unpackControlFile path = unTarGZ path updateDirCurrent

fileRoleFromString "control" = ControlFile
fileRoleFromString other = OtherFile other

fileRoleToString ControlFile = "control"
fileRoleToString (OtherFile other) = other

-- | Do something making sure to revert to Failed update state if things go wrong
savedFailure :: Update a -> Update a
savedFailure action = from =<< try action where
    from (Right r) = return r
    from (Left er) = do
      dbWrite failReasonPath (show er)
      setUpdateState Failed
      throwError er

-- | some assertions
assertUpdateState :: [UpdateState] -> Update ()
assertUpdateState expecting = do
  actual <- getUpdateState
  unless (actual `elem` expecting) . throwError . localE $ InternalError (message expecting actual)
  where
    message exp act = "unexpected update state; expecting " ++ show exp ++ ", have " ++ show act

assertNoGuestVms :: Update ()
assertNoGuestVms = whenM areGuestVmsRunning $
  throwError . localE $ GuestVmsRunning

assertUncorruptFile :: FileDownload -> Update ()
assertUncorruptFile f = unlessM (verifyDownloadedFile f) $
                        throwError . localE $ CorruptFile (downloadPath f)

assertUncorruptFiles :: Update ()
assertUncorruptFiles = mapM_ assertUncorruptFile =<< getDownloadFiles

assertUpdateApplicable :: Update ()
assertUpdateApplicable = do
  applicable <- checkApplicability =<< currentUpdateMeta
  case applicable of
    UpToDate -> return ()
    CannotUpgrade -> throwError $ localE UpdateNotApplicable
    CanUpgrade -> return ()

assertUpdatePolicy :: Update ()
assertUpdatePolicy = unlessM queryUpdatesPolicy $
  throwError $ localE UpgradesPolicyDisabled
