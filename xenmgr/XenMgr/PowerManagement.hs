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

{-# LANGUAGE FlexibleContexts #-}
module XenMgr.PowerManagement (
                         initPm
                       , isLidClosed
                       , isAcAdapter
                       , getBatteryLevel
                       , getVmsToDehibernate
                       , PMAction (..)
                       , PMSettings (..)
                       , executePmAction
                       , pmGetSettings
                       , pmSaveSettings
                       , pmSaveBatteryLidCloseAction
                       , pmSaveAcLidCloseAction
                       , pmActionToStr
                       , pmActionOfStr
                       , pmShutdownVms
                       , hostWhenIdle
                       , hostWhenIdleDoWithState

                       , pmGetScreenRestoreVm
                       , pmSetScreenRestoreVm
                       , pmClearScreenRestoreVm
                       ) where

import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Applicative
import Control.Concurrent
import Control.Exception
import System.Process (readProcessWithExitCode)

import Data.String
import Data.Maybe
import Data.IORef
import Data.List (nub)
import qualified Data.Text.Lazy as TL

import Tools.Log
import Tools.XenStore
import Tools.File
import Tools.Misc
import Tools.Process
import Tools.IfM
import Tools.Future

import XenMgr.Errors
import XenMgr.Config
import XenMgr.Db
import XenMgr.Vm
import XenMgr.User
import XenMgr.Host
import XenMgr.XM
import Vm.Queries
import Vm.State
import XenMgr.Rpc
import Rpc.Autogen.SurfmanClient
import qualified XenMgr.Connect.Xl as Xl
import XenMgr.Connect.InputDaemon

data PMAction = ActionSleep
              | ActionHibernate
              | ActionShutdown
              | ActionForcedShutdown
              | ActionReboot
              | ActionNothing
              | ActionInvalid
              deriving (Eq, Show)

data PMSettings = PMSettings { pmLidCloseACAction      :: PMAction
                             , pmLidCloseBatteryAction :: PMAction }
                deriving (Eq, Show)

data BatteryLevel = BatNormal | BatWarning | BatLow | BatCritical
                  deriving (Eq, Show)

instance Marshall PMAction where
    dbRead  path   = dbReadStr path >>= (\act -> let pmact = (pmActionOfStr act) in
                                                 case pmact of
                                                   ActionInvalid -> do
                                                                     liftIO $ warn ("Incorrect pm action specification: " ++ act)
                                                                     return ActionNothing 
                                                   _             -> return pmact )
    dbWrite path a = dbWriteStr path (pmActionToStr a)

instance Marshall PMSettings where
    dbRead path = do
        acLidAction <- dbReadWithDefault ActionNothing (path ++ "/" ++ "ac-lid-close-action")
        batteryLidAction <- dbReadWithDefault ActionNothing (path ++ "/" ++ "battery-lid-close-action")
        return $ PMSettings { pmLidCloseACAction = acLidAction
                            , pmLidCloseBatteryAction = batteryLidAction }

    dbWrite path v = do dbWrite (path ++ "/" ++ "ac-lid-close-action")      $ pmLidCloseACAction v
                        dbWrite (path ++ "/" ++ "battery-lid-close-action") $ pmLidCloseBatteryAction v

pmActionOfStr "sleep"     = ActionSleep
pmActionOfStr "hibernate" = ActionHibernate
pmActionOfStr "shutdown"  = ActionShutdown
pmActionOfStr "forced-shutdown" = ActionForcedShutdown
pmActionOfStr "reboot"    = ActionReboot
pmActionOfStr "nothing"   = ActionNothing
pmActionOfStr ""          = ActionNothing
pmActionOfStr bad         = ActionInvalid

pmActionToStr ActionSleep     = "sleep"
pmActionToStr ActionHibernate = "hibernate"
pmActionToStr ActionShutdown  = "shutdown"
pmActionToStr ActionForcedShutdown = "forced-shutdown"
pmActionToStr ActionReboot    = "reboot"
pmActionToStr ActionNothing   = "nothing"
pmActionToStr ActionInvalid   = "nothing"

pmGetSettings :: Rpc PMSettings
pmGetSettings = dbReadWithDefault PMSettings { pmLidCloseACAction = ActionNothing
                                             , pmLidCloseBatteryAction = ActionNothing }
                                  "/power-management"

pmSaveSettings :: PMSettings -> Rpc ()
pmSaveSettings s = dbWrite "/power-management" s

pmSaveBatteryLidCloseAction :: PMAction -> Rpc ()
pmSaveBatteryLidCloseAction = dbWrite "/power-management/battery-lid-close-action"

pmSaveAcLidCloseAction :: PMAction -> Rpc ()
pmSaveAcLidCloseAction = dbWrite "/power-management/ac-lid-close-action"

hostStateOfPmAction :: PMAction -> HostState
hostStateOfPmAction ActionSleep = HostGoingToSleep
hostStateOfPmAction ActionHibernate = HostGoingToHibernate
hostStateOfPmAction ActionReboot = HostRebooting
hostStateOfPmAction ActionShutdown = HostShuttingDown
hostStateOfPmAction ActionForcedShutdown = HostShuttingDown
hostStateOfPmAction ActionNothing = HostIdle
hostStateOfPmAction ActionInvalid = HostIdle

-- Do an operation if the host is idle
hostWhenIdle :: (MonadRpc e m) => m () -> m ()
hostWhenIdle action = getHostState >>= maybeExec
    where maybeExec HostIdle = action
          maybeExec _        = return ()

-- Do an operation which is allowed only if the host is idle (resetting it to idle afterwards)
hostWhenIdleDoWithState :: (MonadRpc e m) => HostState -> m () -> m ()
hostWhenIdleDoWithState newState action =
    hostWhenIdle $
      (do setHostState newState
          action
          setHostState HostIdle
       ) `catchError` (\e -> setHostState HostIdle >> throwError e)

isLidClosed :: IO Bool
isLidClosed = xsRead "/pm/lid_state" >>=  pure . f where f (Just "0") = True
                                                         f _          = False

isAcAdapter :: IO Bool
isAcAdapter = xsRead "/pm/ac_adapter" >>= pure . f where f (Just "1") = True
                                                         f _          = False

getBatteryLevel :: IO BatteryLevel
getBatteryLevel =
    xsRead "/pm/currentbatterylevel" >>= pure . f
  where
    f (Just "0") = BatNormal
    f (Just "1") = BatWarning
    f (Just "2") = BatLow
    f (Just "3") = BatCritical
    f _          = BatNormal

isAsleep :: Uuid -> Rpc Bool
isAsleep uuid = (== 3) <$> getVmAcpiState uuid

getCurrentPmAction :: Rpc (Maybe PMAction)
getCurrentPmAction =
    liftIO $ xsRead "/xenmgr/pm-current-action" >>= return . f
  where
    f (Just "sleep")     = Just ActionSleep
    f (Just "hibernate") = Just ActionHibernate
    f (Just "shutdown")  = Just ActionShutdown
    f (Just "forced-shutdown") = Just ActionForcedShutdown
    f (Just "reboot")    = Just ActionReboot
    f _                  = Nothing


setCurrentPmAction :: PMAction -> Rpc ()
setCurrentPmAction action =
    liftIO $ xsWrite "/xenmgr/pm-current-action" (str action)
  where
    str ActionReboot = "reboot"
    str ActionSleep = "sleep"
    str ActionHibernate = "hibernate"
    str ActionShutdown = "shutdown"
    str ActionForcedShutdown = "forced-shutdown"
    str ActionNothing = error "bad pm action"
    str ActionInvalid = error "bad pm action"

clearCurrentPmAction :: Rpc ()
clearCurrentPmAction =
    liftIO $ xsRm "/xenmgr/pm-current-action"

hdxRunning :: Rpc Bool
hdxRunning = (/= []) <$> getRunningHDX

pmGetScreenRestoreVm :: Rpc (Maybe Uuid)
pmGetScreenRestoreVm = dbMaybeRead "/xenmgr/pm-visible-vm"

pmClearScreenRestoreVm :: Rpc ()
pmClearScreenRestoreVm = dbRm "/xenmgr/pm-visible-vm"

-- doesn't store if any HDX vm running
pmSetScreenRestoreVm :: Uuid -> Rpc ()
pmSetScreenRestoreVm vm
  = --whenM (not <$> hdxRunning) $
      dbWrite "/xenmgr/pm-visible-vm" vm

storeVisibleVm :: Rpc ()
storeVisibleVm = from =<< getVisibleVms where
  from (vm:_) = pmSetScreenRestoreVm vm
  from _ = return ()

restoreVisibleVm :: Rpc Bool
restoreVisibleVm = from =<< pmGetScreenRestoreVm where
  from (Just vm) = pmClearScreenRestoreVm >> switchVm vm
  from _ = return False

pmShutdownVms :: Bool -> Rpc ()
pmShutdownVms force = do
    vms <- getVmShutdownOrder
    parallelVmExecInStages vms maybeShutdown
    return ()
  where
    maybeShutdown uuid | force = whenM (isRunning uuid) $ forceShutdownVm uuid
                       | otherwise = do
      acpi <- getVmAcpiState uuid
      when (acpi == 3) $
           do resumed <- liftIO $ resumeFromSleep uuid
              when (not resumed) $ failResumeFromSleep
      running <- isRunning uuid
      when running $
           do t <- getVmGraphics uuid
              when (t == HDX) (switchVm uuid >> return())
              -- ensure the VM is considered dead internally (i.e. shutdown even handlers have ran)
              -- by waiting for internal Shutdown state upto 3 secs
              (shutdownVm uuid >> waitForVmInternalState uuid Shutdown Shutdown 3)
                `catchError` shutdownError

    --FIXME! : should really translate xenvm errors into something better than strings
    shutdownError err
        | show err == "103:VM didn't shutdown as expected." = return ()
        | otherwise = throwError err

-- common code for shutdown/reboot
shutdownCommon :: String -> Bool -> XM ()
shutdownCommon offCommand force = do
    liftRpc $ pmShutdownVms force
    liftIO $ do
      info "PM: halting host now"
      spawnShell offCommand
      -- wait for actuall poweroff
      threadDelay $ 360 * (10^6)

splitHDX :: [Uuid] -> Rpc ([Uuid], [Uuid])
splitHDX uuids = go [] [] uuids where
    go no_hdx hdx []           = return (no_hdx,hdx)
    go no_hdx hdx (uuid:uuids) =
        do g <- getVmGraphics uuid
           case g of
             HDX -> go no_hdx ( hdx ++ [uuid] ) uuids
             _   -> go ( no_hdx ++ [uuid] ) hdx uuids

onScreenHDX :: (MonadRpc e m) => Uuid -> m a -> m a
onScreenHDX uuid f
  = go =<< getVmGraphics uuid where
    go HDX = do
        success <- reallySwitchVm uuid 10
        when (not success) $ warn "FAILED to switch to HDX vm before putting it into S3"
        f
    go _   = f

-- returns True if vm was put to sleep here, False if that was not necessary (because
-- it already was in S3 for example)
putS3 :: Uuid -> XM Bool
putS3 uuid = putS3' uuid =<< liftRpc (getVmS3Mode uuid)

putS3' uuid S3Ignore = return True
putS3' uuid S3Pv = liftRpc $ do
  acpi <- getVmAcpiState uuid
  if (acpi /= 3)
    then onScreenHDX uuid $ do
           info ("PM: putting " ++ show uuid ++ " to sleep") >> sleepVm uuid
           return True
    else   return False
putS3' uuid S3Restart = liftRpc $ do
  acpi <- getVmAcpiState uuid
  if (acpi /= 3)
    then onScreenHDX uuid $ do
           info ("PM: shutting " ++ show uuid ++ " down") >> shutdownVm uuid
           return True
    else   return False

putS3' uuid m = error ("s3 mode " ++ show m ++ " unimplemented")

resumeS3 :: Uuid -> XM ()
resumeS3 uuid = resumeS3' uuid =<< liftRpc (getVmS3Mode uuid)

resumeS3' uuid S3Ignore = return ()
resumeS3' uuid S3Pv = do
  void . liftIO $ Xl.resumeFromSleep uuid
  info $ "PM: Successfully resumed " ++ show uuid ++ " from S3"
resumeS3' uuid S3Restart = do
  startVm uuid
  info $ "PM: Restarted " ++ show uuid ++ " after S3"

resumeS3' uuid S3Snapshot =
  return () -- unimplemented

putS4 :: Uuid -> XM ()
putS4 uuid = putS4' uuid =<< liftRpc (getVmS4Mode uuid)

putS4' uuid S4Ignore = return ()
putS4' uuid S4Pv = liftRpc $ do
  addons <- getVmPvAddons uuid
  -- FIXME: this definately needs cleaning up with respect to which actions require preemptive pv-addons db check,
  -- which require preemptive pv-addons check via hypercall (or xs lookup),
  -- which are fine to fail post-factum
  when (not addons) $ do
    warn $ "PM: VM " ++ show uuid ++ " has no PV addons!"
    failActionRequiresPvAddons
  info $ "PM: attempt to hibernate " ++ show uuid
  hibernateVm uuid
  -- ^ above sets the 'hibernated' db flag for us
putS4' uuid S4Restart = liftRpc $ do
  info ("PM: shutting " ++ show uuid ++ " down")
  shutdownVm uuid
  -- manually toggle S4 flag to induce proper restart action on next host boot
  saveConfigProperty uuid vmHibernated True

putS4' uuid m = error ("s4 mode " ++ show m ++ " unimplemented")

-- We allow only one PM action to be run at the time
executePmAction :: PMAction -> XM ()
executePmAction action = executePmActionInternal action True -- supervised by default
executePmActionInternal :: PMAction -> Bool -> XM ()
executePmActionInternal action supervised =
    when (action /= ActionNothing) $ do
      info $ "PM: received pm action request: " ++ show action
      current <- liftRpc getCurrentPmAction
      case current of
        Just c  -> info $ "PM: but pm action " ++ show c ++ " is currently running, so cannot do"
        Nothing -> (do liftRpc $ setCurrentPmAction action
                       execute_ action supervised
                       liftRpc $ clearCurrentPmAction)
                   `catchError` \err -> do
                           liftRpc $ clearCurrentPmAction
                           throwError err

execute_ ActionReboot supervised = do
    info "PM: received host reboot request"
    shutdownCommon "reboot" False

execute_ ActionShutdown supervised = do
    info "PM: received host shutdown request"
    shutdownCommon "poweroff" False

execute_ ActionForcedShutdown supervised = do
    info "PM: received host force shutdown request"
    setHostState HostShuttingDown
    shutdownCommon "poweroff" True

execute_ ActionSleep supervised = do
    info "PM: received host sleep request"
    queue <- filter (\(uuid,slept) -> slept == True) <$> putVmsToSleep
    liftRpc (whenM inputAuthOnBoot $ info "PM: locking screen" >> inputLock)
    info "PM: expiring user sessions"
    liftRpc expireSessions
    -- info "PM: logging out of synchroniser"
    -- liftRpc $ comCitrixXenclientBedUserLogout "com.citrix.xenclient.bed" "/"
    info "PM: executing surfman pre-s3"
    liftRpc $ comCitrixXenclientSurfmanPreS3 "com.citrix.xenclient.surfman" "/"
    info "PM: executing s3 suspend script"
    liftIO $ spawnShell "/usr/share/xenclient/enter-s3.sh"
    info "PM: resumed host from s3"
    info "PM: executing surfman post-s3"
    liftRpc $ comCitrixXenclientSurfmanPostS3 "com.citrix.xenclient.surfman" "/"
    hdx <- liftRpc getRunningHDX
    -- FIXME: we probably should have separate 'host-resuming-from-sleep-state' here
    liftRpc $ setHostState HostIdle
    -- Resume all vms
    resumeQueue (map fst queue)
    when (hdx == []) $ do
      restored <- liftRpc $ restoreVisibleVm
      when (not restored) $ void $ liftRpc $ switchGraphicsFallback
    liftRpc $ pmClearScreenRestoreVm
  where
    -- stage S3 so non hdx vm go to sleep first in parallel, then all hdx vms
    putVmsToSleep  = xmContext >>= \xm -> liftRpc $ do
      guests <- getGuestVms
      -- FIXME: remove this by making all vms (not just user vms) go thru this pipeline
      let needsS3Restart uuid = (`elem` [ S3Restart ]) <$> getVmS3Mode uuid
      more   <- getVmsBy needsS3Restart
      (no_hdx, hdx) <- splitHDX guests
      parallelVmExecInStages [ no_hdx, hdx, more ] (sleep xm)

    sleep xm uuid = go =<< isRunning uuid where
      go True = runXM xm $ putS3 uuid
      go _    = return False

    resumeQueue    = mapM resumeS3

    expireSessions =
        do users <- enumActiveUsers
           mapM expire users
           where
             expire u = info ("PM: terminating user session " ++ show u) >> expireUserSession u

execute_ ActionHibernate supervised = do
    info "PM: received host hibernate request"
    liftRpc $ maybeSwitch =<< getRunningHDX

    -- execute hibernate request in parallel for all VMS (but pvm always last)
    -- if it bugs, raise nice error message
    parallelHib `catchError` \err -> failHibernateFailed
    -- now hibernated vms are already shut down, run common shutdown code to get rid of service vms
    liftRpc $ dbWrite "/platform/hibernated" "true"
    shutdownCommon "poweroff" False

  where
    maybeSwitch [] = return ()
    maybeSwitch (uuid:_) = switchVm uuid >> return ()

    parallelHib = xmContext >>= \xm -> liftRpc $
        do guests <- getGuestVms
           (no_hdx,hdx) <- splitHDX guests
           parallelVmExecInStages [no_hdx,hdx] (attempt xm)

    attempt xm uuid = go =<< isRunning uuid where
      go True = (runXM xm $ putS4 uuid) `catchError` on_vm_hib_error uuid
      go _ = return ()

    -- what to do on VM hibernate error:
    --   If we are supervised by the user, propagate exception during hibernate.
    --   otherwise, swallow it, shutdown VM and continue with hibernate sequence
    on_vm_hib_error uuid err | supervised == True  = do warn $ "PM: problem when trying to hibernate " ++ show uuid ++ ", aborting sequence"
                                                        throwError err
                             | otherwise           = do warn $ "PM: problem when trying to hibernate, shutting it down instead: " ++ show uuid ++ ": " ++ show err
                                                        shutdownVm uuid


execute_ ActionNothing supervised = return ()
execute_ ActionInvalid supervised = return ()

isTXTLaunch :: IO Bool
isTXTLaunch =
    do (_, out, _) <- readProcessWithExitCode "txt-stat" [] ""
       let out' = TL.pack out
       case (TL.find flag'a out', TL.find flag'b out') of
         ((_, ma), (_, mb)) | not (null ma) && not (null mb)
             -> return True
         _   -> return False
    where
      flag'a = TL.pack $ "TXT measured launch: TRUE"
      flag'b = TL.pack $ "secrets flag set: TRUE"

assertNotTXTLaunch :: Rpc ()
assertNotTXTLaunch = from =<< liftIO isTXTLaunch where
    from False = return ()
    from _ = failCannotSleepInTXTMeasuredLaunch

-- Resume all vms from hibernation
getVmsToDehibernate :: Rpc [Uuid]
getVmsToDehibernate = do
    -- only do this if autostart enabled
    autostart <- appAutoStart
    if not autostart
       then return []
       else getGuestVms >>=
            filterM getVmHibernated >>=
            filterM (\uuid -> not <$> isRunning uuid)

type EventHandler = XM ()
type Event        = (String,String)

handleLidStateChanged closed = do
    debug $ "PM: detected lid state change event, closed=" ++ (show closed)
    when closed $ do
      a <- action
      hostWhenIdleDoWithState (hostStateOfPmAction a) $ do
        when (a `elem` [ActionHibernate, ActionSleep]) $
          liftRpc $ storeVisibleVm
        executePmActionInternal a False
  where
    action = do
      settings <- liftRpc pmGetSettings
      ac       <- liftIO isAcAdapter
      return $ case ac of
        True   -> pmLidCloseACAction settings
        False  -> pmLidCloseBatteryAction settings

handlePowerButtonPressed = do
    debug "PM: detected power button press event"
    hostWhenIdleDoWithState HostShuttingDown $ executePmActionInternal ActionShutdown True

handleSleepButtonPressed = do
    debug "PM: detected sleep button press event"
    hostWhenIdleDoWithState HostGoingToSleep $ do
      liftRpc storeVisibleVm
      executePmActionInternal ActionSleep True

handleBatteryLevelChanged = do
    level <- liftIO $ getBatteryLevel
    debug $ "PM: detected battery level change event = " ++ (show level)
    when (level == BatCritical) $
         hostWhenIdleDoWithState HostGoingToHibernate $ do
           liftRpc storeVisibleVm
           executePmActionInternal ActionHibernate False

whenEvent :: Event -> EventHandler -> XM ()
whenEvent (intf,signal) h =
    let rule = matchSignal intf signal
    in
      xmContext >>= \c -> liftRpc $ rpcOnSignal rule (process c)
  where
    process c _ signal = runXM c (void $ future h)

lidStateChanged          = ("com.citrix.xenclient.input", "lid_state_changed")
powerButtonPressed       = ("com.citrix.xenclient.xcpmd", "power_button_pressed")
sleepButtonPressed       = ("com.citrix.xenclient.xcpmd", "sleep_button_pressed")
batteryLevelNotification = ("com.citrix.xenclient.xcpmd", "battery_level_notification")

initPm :: XM ()
initPm = do
    whenEvent lidStateChanged $ do
      closed <- liftIO isLidClosed
      handleLidStateChanged closed

    whenEvent powerButtonPressed       (handlePowerButtonPressed)
    whenEvent sleepButtonPressed       (handleSleepButtonPressed)
    whenEvent batteryLevelNotification (handleBatteryLevelChanged)
    info "installed PM event handlers"

