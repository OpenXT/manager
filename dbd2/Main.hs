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
{-# LANGUAGE CPP #-}
module Main where

import qualified Data.ByteString as B
import System.Exit
import System.Environment
import System.Posix.Signals
import System.Posix.Syslog
import System.Posix.Process
import System.IO
import System.Console.GetOpt
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

import Text.JSON
import qualified Store
import qualified Tree
import Control.Applicative
import qualified Control.Exception as E
import Tools.Log

import Data.Int
import Data.List (isPrefixOf)

import Rpc.Autogen.DbServer
import Rpc.Core
import Rpc
import Utils
import Path (pathOf, PathCred)

#ifndef TESTING
import Rpc.Autogen.DbusClient (orgFreedesktopDBusGetConnectionDOMID)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Lazy as TL
import System.Xen.Store
#endif

implementation readUuid notifyMe = DbServer
    { comCitrixXenclientDbRead       = withPath readUuid doRead
    , comCitrixXenclientDbReadBinary = withPath readUuid doReadBinary
    , comCitrixXenclientDbWrite      = doWrite readUuid notifyMe
    , comCitrixXenclientDbDump       = withPath readUuid doDump
    , comCitrixXenclientDbInject     = doInject readUuid notifyMe
    , comCitrixXenclientDbList       = withPath readUuid doList
    , comCitrixXenclientDbRm         = withPath readUuid (doRm notifyMe)
    , comCitrixXenclientDbExists     = withPath readUuid doExists
#ifdef PERM
    , comCitrixXenclientDbChmod      = doChmod readUuid notifyMe
    , comCitrixXenclientDbLsmod      = withPath readUuid doLsmod
#endif
    }

data Debugging = DebugNone | DebugSyslog | DebugFile
    deriving (Eq)

debugging = DebugNone

printDebug msg r
    | debugging == DebugFile = appendFile "/config/db.log" (msg (" = " ++ r ++ "\n"))
    | otherwise              = return ()

executeIO :: (Show a, MonadIO m) => String -> PathCred -> IO a -> m a
executeIO cmd path f = liftIO ((f >>= \r -> (printDebug msg (show r) >> return r)) `E.catch` logAndRaise)
    where msg p = cmd ++ ": " ++ show path ++ p
          logAndRaise :: E.SomeException -> IO a
          logAndRaise e = printDebug msg (show e) >> E.throwIO e

withPath readUuid f p = toPath readUuid p >>= f

toPath readUuid s = do
    msender <- rpcGetSender
    case msender of
        Nothing     -> return (pathOf s, Nothing)
        Just sender -> do domid <- getConnectionDOMID sender
                          case domid of
                               0 -> return (pathOf s, Nothing)
                               _ -> do muuid <- liftIO (readUuid domid)
                                       case muuid of
                                            Nothing   -> error "cannot resolve uuid"
                                            Just uuid | "abs:" `isPrefixOf` s -> error "not implemented"
                                                      | otherwise             -> return (pathOf ("/dom-store/" ++ show uuid ++ "/" ++ s), Just uuid)
    where
#ifdef TESTING
          getConnectionDOMID _      = return 0
#else
          getConnectionDOMID sender = orgFreedesktopDBusGetConnectionDOMID "org.freedesktop.DBus" "/org/freedesktop/DBus" (TL.unpack $ strBusName sender)
#endif

doRead path = maybe "" printJSON <$> executeIO "read" path (   Store.checkReadPerm path
                                                            >> Store.withTree (Tree.valueGet (fst path)))
    where printJSON :: JSValue -> String
          printJSON (JSString s) = fromJSString s
          printJSON z            = encode z

doReadBinary path = liftIO (   Store.checkReadPerm path
                            >> Store.withTree (Tree.getSubtree (fst path))
                           ) >>= readFilePtr
    where readFilePtr (Tree.FilePtr file) = liftIO $ B.readFile file
          readFilePtr _                   = error "not a binary node"

doWrite readUuid notify p v = do
    path <- toPath readUuid p
    liftIO $ Store.modifyTree (Tree.valueSet (fst path) (JSString $ toJSString v)) >> notify

doDump path = pretty . Tree.toJSON <$> (liftIO $ Store.withTree $ Tree.getSubtree (fst path))

doInject readUuid notify p v = do
    path <- toPath readUuid p
    case parseJSON v of
        Left err   -> error ("cannot parse json value: " ++ show err)
        Right jtree -> executeIO "inject" path (   Store.checkWritePerm path
                                                >> Store.modifyTree (Tree.inject (fst path) $ Tree.ofJSON jtree)
                                               ) >> (liftIO notify)

doList path = marshall <$> (executeIO "list" path $ Store.withTree $ Tree.getSubtree (fst path))
    where marshall (Tree.Nodes ns) = map fst ns
          marshall _               = []

doRm notify path = executeIO "rm" path (Store.checkWritePerm path >> Store.modifyTree (Tree.rm (fst path)) >> notify)

doExists path = executeIO "exists" path (Store.checkReadPerm path >> Store.withTree (Tree.exists (fst path)))

#ifdef PERM
doChmod readUuid notify p perm = do
    path <- toPath readUuid p
    liftIO $ do
        Store.modifyPerms (Perms.chPerm path (parsePermPatchs perm))
        notify

doLsmod path = liftIO $ Store.withPerms (map show . Perms.getPerms path)
#endif

termHandler, hupHandler :: IO ()
hupHandler = do
    putStrLn "Sighup handler"
    Store.restore

termHandler = do
    putStrLn "Sigterm handler"
    Store.flush
    exitSuccess

data Flag = NoDaemonize
          | NoAutostart
          | WritePid (Maybe String)
          | OptDebug
          | User
          | Help
          deriving Eq

options =
    [ Option ""  ["no-daeonize"] (NoArg NoDaemonize) "do no daemonize the process"
    , Option ""  ["no-autostart"] (NoArg NoAutostart) "do not autostart vms"
    , Option ""  ["user"] (NoArg User) "use a dbus session bus instead of the system bus"
    , Option ""  ["writepid"] (OptArg WritePid "FILE") "write pid to FILE"
    , Option ""  ["debug"] (NoArg OptDebug) "additional debug info (rpc timings etc)"
    , Option "h" ["help"] (NoArg Help) "print usage information" ]

main :: IO ()
main = do
    args <- getArgs
    let (opts,_,_) = getOpt Permute options args
    when (OptDebug `elem` opts) $ rpcDebug True
    startup opts doMain
    where startup opts f
                 | Help `elem` opts = do prg <- getProgName
                                         hPutStrLn stderr (usageInfo prg options)
                                         exitWith ExitSuccess
                 | NoDaemonize `elem` opts = do pid <- getProcessID
                                                dumpPid opts pid
                                                runMe (f (User `elem` opts))
                 | otherwise = do pid <- forkProcess (runMe (f (User `elem` opts)))
                                  dumpPid opts pid
          runMe f = withSyslog "dbd" [] USER f

          dumpPid opts pid = case fname opts of
                                      Just path -> writeFile path $ show pid ++ "\n"
                                      Nothing   -> return ()
                where
                          fname [] = Nothing
                          fname ((WritePid p) : _) = p
                          fname (_:xs) = fname xs

doMain user = do
    Store.restore

    _ <- installHandler sigHUP (Catch hupHandler) Nothing
    _ <- installHandler sigTERM (Catch termHandler) Nothing

    chan <- newChan

#ifdef TESTING
    let readUuid :: Int32 -> IO (Maybe Uuid)
        readUuid domid = return Nothing
#else
    xsh <- initiateXS
    let xsReadOpt p = E.catchJust (\e -> if e == ErrorNoEnt then Just () else Nothing)
                                  (Just <$> xsRead xsh p)
                                  (\_ -> return Nothing)
    let readUuid domid = do
        p <- xsReadOpt $ BC.pack ("/local/domain/" ++ show domid ++ "/vm")
        r <- case p of
                  Nothing     -> return Nothing
                  Just vmPath -> fmap (Uuid . BC.unpack) <$> xsReadOpt (vmPath `B.append` BC.pack "/uuid")
        printDebug (("read uuid: " ++ show domid ++ " :") ++) (show r)
        return r
#endif

    rpcServeOn user service $ \rpcContext -> do
        r <- E.try $ do
            status <- rpc rpcContext $ do
                expose (implementation readUuid (writeChan chan ()))
                liftIO . forever $ do
                    () <- readChan chan
                    -- delay the writing by 5s, flush all other queued update
                    threadDelay (5 * 1000000)
                    whileM (not <$> isEmptyChan chan) (readChan chan >> return ())
                    Store.flush
            case status of
                Left e  -> fatal $ "error during initialisation " ++ show e
                Right _ -> return ()
        case r of
                Right () -> return ()
                Left (ex :: E.SomeException) -> fatal $ "Caught error: " ++ show ex
