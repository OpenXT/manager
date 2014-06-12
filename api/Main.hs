{-# LANGUAGE ScopedTypeVariables,EmptyDataDecls #-}

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
import qualified Data.Map as M

import qualified Data.Text.Lazy as TL

import Rpc
import Rpc.Core
import App
import Tools.Log
import Error

import Data.Int
import Data.String
import Rpc.Autogen.ApiHostServer hiding (interfaces)
import qualified Rpc.Autogen.ApiHostServer as H

import Rpc.Autogen.ApiVmServer hiding (interfaces)
import qualified Rpc.Autogen.ApiVmServer as V

import Rpc.Autogen.ApiProductServer hiding (interfaces)
import qualified Rpc.Autogen.ApiProductServer as P

import Rpc.Autogen.XenmgrHostClient
import Rpc.Autogen.XenmgrVmClient
import Rpc.Autogen.XenmgrClient
import Rpc.Autogen.VmDiskClient


import Control.Monad.Trans
import Control.Monad.Error

main :: IO ()
main = do doService apiService $ implementApiHostServer
                               : implementApiVmServer
                               : implementApiProductServer
                               : []

doService service implementations =
    withSyslog "xenclient-api" [] USER . rpcServe service $ \rpcContext ->
        E.try (do debug "Starting.."
                  app_state <- initAppState
                  mapM_ (handleInnerStatus <=< rpc rpcContext . runApp app_state) implementations
                  -- live forever
                  liftIO . forever $ threadDelay (10^6 * 60))
        >>= handleOuterStatus
    where
      handleInnerStatus = handleLeft $ \error -> fatal $ "Error during initialisation " ++ show error
      handleOuterStatus = handleLeft $ \(ex :: E.SomeException) -> fatal $ "Caught error: " ++ show ex
      handleLeft :: (Monad m) => (a -> m ()) -> Either a b -> m ()
      handleLeft op = either op (const $ return ())

eitherErr a = (Right <$> a) `catchError` (\the_error -> return . Left $ the_error)

xenmgrService = "com.citrix.xenclient.xenmgr"

apiService = "com.citrix.xenclient.api.v0"

implementApiHostServer :: App ()
implementApiHostServer = do
  liftRpc $ rpcExpose (fromString "/host") . H.interfaces $ ApiHostServer
    { comCitrixXenclientApiHostShutdown = fix comCitrixXenclientXenmgrHostShutdown
    , comCitrixXenclientApiHostReboot = fix comCitrixXenclientXenmgrHostReboot
    , comCitrixXenclientApiHostSleep = fix comCitrixXenclientXenmgrHostSleep
    , comCitrixXenclientApiHostHibernate = fix comCitrixXenclientXenmgrHostHibernate
    , comCitrixXenclientApiHostGetTotalMem = fix comCitrixXenclientXenmgrHostGetTotalMem
    , comCitrixXenclientApiHostGetFreeMem = fix comCitrixXenclientXenmgrHostGetFreeMem
    , comCitrixXenclientApiHostGetTotalStorage = fix comCitrixXenclientXenmgrHostGetTotalStorage
    , comCitrixXenclientApiHostGetFreeStorage = fix comCitrixXenclientXenmgrHostGetFreeStorage
    , comCitrixXenclientApiHostGetCpuCount = fix comCitrixXenclientXenmgrHostGetCpuCount
    , comCitrixXenclientApiHostGetModel = fix comCitrixXenclientXenmgrHostGetModel
    , comCitrixXenclientApiHostGetVendor = fix comCitrixXenclientXenmgrHostGetVendor
    , comCitrixXenclientApiHostGetBiosRevision = fix comCitrixXenclientXenmgrHostGetBiosRevision
    , comCitrixXenclientApiHostGetPhysicalCpuModel = fix comCitrixXenclientXenmgrHostGetPhysicalCpuModel
    , comCitrixXenclientApiHostGetPhysicalGpuModel = fix comCitrixXenclientXenmgrHostGetPhysicalGpuModel
    }
  where -- fix common arguments:
        fix f = f xenmgrService "/host"

-- ApiDiskServer
-- NicServer

implementApiVmServer =
  liftRpc $ rpcExpose (fromString "/vm") . V.interfaces $ ApiVmServer
    { comCitrixXenclientApiVmFindVmByUuid = s comCitrixXenclientXenmgrFindVmByUuid "/"
    , comCitrixXenclientApiVmCreateVm = s comCitrixXenclientXenmgrCreateVm "/"
    , comCitrixXenclientApiVmGetName = su comCitrixXenclientXenmgrVmGetName
    , comCitrixXenclientApiVmSetName = su comCitrixXenclientXenmgrVmSetName
    , comCitrixXenclientApiVmGetDescription = su comCitrixXenclientXenmgrVmGetDescription
    , comCitrixXenclientApiVmSetDescription = su comCitrixXenclientXenmgrVmSetDescription
    -- Make it a method, because it's kind of expensive to call, and change name to readIconPixels.
    , comCitrixXenclientApiVmGetIcon = su comCitrixXenclientXenmgrVmReadIcon
    , comCitrixXenclientApiVmAddDisk = \vm size ->
    (do (diskObjPath :: ObjectPath) <- s comCitrixXenclientXenmgrVmAddDisk vm
        (vhdPath :: String) <- s comCitrixXenclientXenmgrCreateVhd "/" size
        -- ToDo: Fix the need to use strObjectPath in RPC generator.
        s comCitrixXenclientVmdiskAttachVhd (TL.unpack $ strObjectPath diskObjPath) vhdPath
        return diskObjPath)
    , comCitrixXenclientApiVmSetCdrom = su comCitrixXenclientXenmgrVmSetCd
    , comCitrixXenclientApiVmGetWiredNetwork = su comCitrixXenclientXenmgrVmGetWiredNetwork
    , comCitrixXenclientApiVmSetWiredNetwork = su comCitrixXenclientXenmgrVmSetWiredNetwork
    , comCitrixXenclientApiVmGetHasTools = su comCitrixXenclientXenmgrVmGetPvAddons
    , comCitrixXenclientApiVmGetAutostart = su comCitrixXenclientXenmgrVmGetStartOnBoot
    , comCitrixXenclientApiVmSetAutostart = su comCitrixXenclientXenmgrVmSetStartOnBoot
    , comCitrixXenclientApiVmSwitch = su comCitrixXenclientXenmgrVmSwitch
    , comCitrixXenclientApiVmStart = su comCitrixXenclientXenmgrVmStart
    , comCitrixXenclientApiVmReboot = su comCitrixXenclientXenmgrVmReboot
    , comCitrixXenclientApiVmShutdown = su comCitrixXenclientXenmgrVmShutdown
    , comCitrixXenclientApiVmHibernate = su comCitrixXenclientXenmgrVmHibernate
    , comCitrixXenclientApiVmGetListVms = s comCitrixXenclientXenmgrListVms "/"
    -- Remove templates, and only call it getImages for examples, and
    -- not listImages.
    , comCitrixXenclientApiVmListTemplates = s comCitrixXenclientXenmgrListTemplates "/"
    -- , comCitrixXenclientApiVmGetListVmImage = undefined -- :: m ([String])
    -- don't list wired networks, if at all, list wired network interfaces.
    -- Call them nics.
    -- , comCitrixXenclientApiVmGetListWiredNetworks = undefined -- :: m ([String])
    -- , comCitrixXenclientApiVmGetListCdroms = undefined -- :: m ([String])
    }
  where s f = f xenmgrService
        su f = s f . vmObjPath
        -- ToDo: Consider importing this function from XenMgr, instead of copying.
        vmObjPath :: String -> String
        vmObjPath uuid = "/vm/" ++ uuidStrUnderscore uuid

        uuidStrUnderscore :: String -> String
        uuidStrUnderscore =
          map subst
          where
            subst '-' = '_'
            subst ch  = ch

--- <property name="build-info" type="a{ss}" access="read">

implementApiProductServer =
  liftRpc $ rpcExpose (fromString "/product") . P.interfaces $ ApiProductServer
    { comCitrixXenclientApiProductGetVersion = lookupBuildInfo "Error: No version number fonud." "version"
    , comCitrixXenclientApiProductGetBuild = lookupBuildInfo "Error: No build number found." "build"
    , comCitrixXenclientApiProductGetToolsVersion = lookupBuildInfo "Error: No tools version found" "tools"
    }
  where lookupBuildInfo default_ what =
            liftM (M.findWithDefault default_ what) $
            comCitrixXenclientXenmgrHostGetBuildInfo xenmgrService "/host"
