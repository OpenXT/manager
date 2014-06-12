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

module Main where

import Data.String
import Control.Applicative
import System
import System.IO
import System.Posix.Syslog
import System.Posix.Process
import System.Posix.Signals
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad

import Rpc
import Tools.Log
import Tools.XenStore
import Db
import Types
import VhdSync
import DiskSync
import DiskGroup
import Disk
import Image
import qualified ImageRpc
import qualified DiskRpc
import qualified DiskGroupRpc
import qualified MainRpc

-- We need these services in order to start xenmgr at all
prerequisites :: [String]
prerequisites =
    [
     "com.citrix.xenclient.db"
    ]

waitPrerequisites :: Rpc ()
waitPrerequisites = mapM_ rpcWaitForService prerequisites

-- Watch for network connectivity
-- When the network comes up -> run onNetUp
watchNetworkStatus :: RpcContext -> Rpc () -> Bool -> IO ()
watchNetworkStatus context onNetUp stop = watcher >> return ()
 where
    watcher = do xsWaitFor "/xenmgr/network-available" (rpc context check >>= return . continue)
                 if stop then return () else watcher
    continue (Left _)      = False
    continue (Right netUp) = stop && netUp

    check :: Rpc Bool
    check        = debug "testing network status.." >> checkNetwork
    checkNetwork = do
      info "detected network status change"
      netAvailableStr <- liftIO $ xsRead ("/xenmgr/network-available")
      let netUp = netAvailableStr == Just "1"
      when ( netUp ) $ info "network up" >> onNetUp
      return netUp

startTasks :: AppContext -> (GroupState -> Bool) -> (GroupId -> DiskSync ()) -> Rpc ()
startTasks ctx startWhen start =
    runDiskSync ctx $ mapM_ start =<< filterM ((liftM startWhen) . groupGetState) =<< groupList

sendTaskStates :: AppContext -> (GroupState -> Bool) -> Rpc ()
sendTaskStates ctx sendWhen =
    runDiskSync ctx $ mapM_ sendGroupStateChange' =<< filterM ((liftM sendWhen) . groupGetState) =<< groupList

stopAllTasks :: AppContext -> Rpc ()
stopAllTasks ctx =
    runDiskSync ctx $ mapM_ groupStop =<< groupList

-- reset inprogress task states on daemon start (a task is considered inprogress if vhd-sync is alive)
-- this assumes no tasks are xferring when daemon starts, which is the expected state of things
clearProgressStates :: DiskSync ()
clearProgressStates = mapM_ reset =<< filterM inprogress =<< imageList where
    inprogress t = return . not . stopped =<< imageGetState t

    stopped ImageStopped    = True
    stopped ImageFinished   = True
    stopped (ImageFailed _) = True
    stopped _               = False

    reset image = do
      warn $ "resetting in-progress image state for task: " ++ show image
      imageSetState image ImageStopped

main :: IO ()
main = do
    app <- createAppContext
    withStdOutLog "/tmp/disksyncmgr.out" $
      withSyslog "disksyncmgr" [] USER . rpcServeWithCustomStop "com.citrix.xenclient.disksyncmgr" (stop app) $ \rpcContext ->
        do r <- E.try $
                do debug "starting.."
                   status <-
                       rpc rpcContext $
                       do -- wait until prerequisites are connected to dbus
                         info "waiting for prerequisite services to appear on DBUS.."
                         waitPrerequisites
                         runDiskSync app $ do
                           clearProgressStates
                           groups <- groupList
                           disks  <- diskList
                           images <- imageList
                           liftRpc $ do
                             MainRpc.expose app
                             mapM_ (DiskGroupRpc.expose app) groups
                             mapM_ (DiskRpc.expose app) disks
                             mapM_ (ImageRpc.expose app) images

                         -- wait until the network comes up and then restart unfinished tasks
                         liftIO $ watchNetworkStatus rpcContext (restartTasks app) True

                         -- watch network status forever
                         -- resume stalled tasks when the network is available
                         liftIO $ do
                           forkIO $ watchNetworkStatus rpcContext (resumeTasks app) False
                           forever $ threadDelay (10^6 * 60)
                   case status of
                     Left error -> fatal $ "error during initialisation " ++ show error
                     Right _    -> return ()
           case r of
             Right () -> return ()
             Left (ex :: E.SomeException) -> fatal $ "Caught error: " ++ show ex

    where
      restartTasks :: AppContext -> Rpc ()
      restartTasks ctx =
          info "restarting unfinished tasks.." >> startTasks ctx notFinished groupStart
          where
            notFinished = (/=) GroupFinished

      resumeTasks :: AppContext -> Rpc ()
      resumeTasks ctx =
          info "sending state change for finished tasks.." >>
          sendTaskStates ctx finished >>
          info "resuming stalled tasks.." >>
          startTasks ctx stalled groupResume
          where
            finished = (==) GroupFinished
            stalled = (==) GroupNetworkStalled

      stop app ctx = do
        info "stopping tasks.."
        rpc ctx $ stopAllTasks app
        return ()
