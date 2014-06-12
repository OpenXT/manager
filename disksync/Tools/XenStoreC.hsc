--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

{-# LANGUAGE CPP,ForeignFunctionInterface,ScopedTypeVariables #-}
module Tools.XenStoreC (
                         xsRead, xsReadH
                       , xsWrite, xsWriteH
                       , xsRm, xsRmH
                       , xsDir, xsDirH
                       , xsWaitFor, xsWaitForH
                       , Permission(..)
                       , PermissionFlag(..)
                       , xsGetPermissions, xsGetPermissionsH
                       , xsSetPermissions, xsSetPermissionsH
                       ) where

import Control.Monad
import Control.Concurrent
import Control.Applicative
import qualified Control.Exception as E
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import System.Posix
import System.IO.Unsafe
import Tools.Log
import Data.Bits

import Data.Map (Map)
import qualified Data.Map as M

type XsHandle = Ptr ()
data Xs = Xs { handle  :: XsHandle
             , watches :: MVar (Map WatchID WatchLock)
             , watches_reader :: ThreadId }

type WatchLock = MVar ()
type WatchID = String -- equivalent to path in xs

#include <xs.h>
#include <xs_lib.h>

#def typedef struct xs_permissions xs_permissions_t;

foreign import ccall "xs.h xs_daemon_open"
        c_xs_daemon_open :: IO XsHandle

foreign import ccall "xs.h xs_daemon_close"
        c_xs_daemon_close :: XsHandle -> IO ()

foreign import ccall "xs.h xs_read"
        c_xs_read :: XsHandle -> CUInt -> CString -> Ptr CUInt -> IO CString

foreign import ccall "xs.h xs_write"
        c_xs_write :: XsHandle -> CUInt -> CString -> CString -> CUInt -> IO CUInt

foreign import ccall "xs.h xs_rm"
        c_xs_rm :: XsHandle -> CUInt -> CString -> IO CUInt

foreign import ccall "xs.h xs_watch"
        c_xs_watch :: XsHandle -> CString -> CString -> IO CInt

foreign import ccall "xs.h xs_unwatch"
        c_xs_unwatch :: XsHandle -> CString -> CString -> IO CInt

foreign import ccall "xs.h xs_fileno"
        c_xs_fileno :: XsHandle -> IO CInt

foreign import ccall "xs.h xs_read_watch"
        c_xs_read_watch :: XsHandle -> Ptr CUInt -> IO (Ptr CString)

foreign import ccall "xs.h xs_get_permissions"
        c_xs_get_permissions :: XsHandle -> CInt -> CString -> Ptr CInt -> IO (Ptr Permission)

foreign import ccall "xs.h xs_set_permissions"
        c_xs_set_permissions :: XsHandle -> CInt -> CString -> Ptr Permission -> CInt -> IO CInt

foreign import ccall "xs.h xs_directory"
        c_xs_directory :: XsHandle -> CInt -> CString -> Ptr CUInt -> IO (Ptr CString)

foreign import ccall "read_watch_token"
        c_read_watch_token :: XsHandle -> IO CString

xsOpen :: IO Xs
xsOpen =
    do h <- c_xs_daemon_open
       w <- newMVar M.empty
       r <- forkIO $ processWatches h w
       return $ Xs h w r

xsClose :: Xs -> IO ()
xsClose xs =
    do killThread (watches_reader xs)
       c_xs_daemon_close (handle xs)

processWatches :: XsHandle -> MVar (Map WatchID WatchLock) -> IO ()
processWatches h watch_map =
    do fd <- fromIntegral <$> c_xs_fileno h
       threadWaitRead fd
       t <- c_read_watch_token h
       when (t /= nullPtr) $ (process_token t >> free t)
       processWatches h watch_map
    where
      process_token c_t =
          do t <- peekCString c_t
             -- unlock coresponding watch by stuffing mvar
             watches <- readMVar watch_map
             case M.lookup t watches of
               Nothing -> return () -- nobody watching this
               Just l  -> tryPutMVar l () >> return () -- unlock watch

-- hackish single global xenstore handle
xsGlobal :: Xs
{-# NOINLINE xsGlobal #-}
xsGlobal = unsafePerformIO $ xsOpen

--FIXME: throw exception on bad return codes

xsWriteH :: Xs -> String -> String -> IO ()
xsWriteH h path value =
    withCString path $ \pathS ->
        withCString value $ \valueS ->
            do ret <- c_xs_write (handle h) (fromIntegral 0) pathS valueS (fromIntegral $ length value)
               return ()

xsReadH :: Xs -> String -> IO (Maybe String)
xsReadH h path =
        withCString path $ \pathS ->
            alloca $ \lenP ->
                do ret <- c_xs_read (handle h) (fromIntegral 0) pathS lenP
                   if ret == nullPtr
                      then return Nothing
                      else do str <- peekCString ret
                              str `seq` free ret
                              return . Just $ str

xsRmH :: Xs -> String -> IO ()
xsRmH h path =
        withCString path $ \pathS ->
            do ret <- c_xs_rm (handle h) (fromIntegral 0) pathS
               return ()

xsWatchH :: Xs -> String -> String -> IO ()
xsWatchH h path token =
    withCString path $ \pathS ->
        withCString token $ \tokenS ->
            do ret <- c_xs_watch (handle h) pathS tokenS
               lock <- newEmptyMVar
               modifyMVar_ (watches h) $ \m -> return $ M.insert token lock m

xsUnwatchH :: Xs -> String -> String -> IO ()
xsUnwatchH h path token =
        withCString path $ \pathS ->
            withCString token $ \tokenS ->
                do ret <- c_xs_unwatch (handle h) pathS tokenS
                   modifyMVar_ (watches h) $ \m -> return $ M.delete token m

xsFilenoH :: Xs -> IO Fd
xsFilenoH h =
        do f <- c_xs_fileno (handle h)
           return $ fromIntegral f

xsWaitForH :: Xs -> String -> IO Bool -> IO ()
xsWaitForH h path pred =
    do xsWatchH h path path
       debug ("xs -> watching for " ++ show path)
       watch `E.catch` (\(e::E.SomeException) -> xsUnwatchH h path path)
    where
       watch = do
         m <- readMVar (watches h)
         -- path used as token
         let Just w = M.lookup path m
         -- block until change, check predicate
         takeMVar w
         -- it fired
         debug $ "xs -> watch " ++ path ++ " fired"
         p <- pred
         case p of
           True -> do debug ("xs -> watch " ++ path ++ " predicate tested positive")
                      xsUnwatchH h path path
           _    -> do debug ("xs -> watch " ++ path ++ " predicate tested negative, continuing..")
                      watch

xsDirH :: Xs -> String -> IO [String]
xsDirH h path =
    withCString path $ \pathS ->
        alloca $ \num_p ->
            do ptrs <- c_xs_directory (handle h) (fromIntegral 0) pathS num_p
               num  <- peek num_p
               if (ptrs == nullPtr)
                  then return []
                  else do strs <- parsePtrArray num ptrs
                          free ptrs
                          return strs
    where
      parsePtrArray 0 ps = return []
      parsePtrArray n ps =
          do x  <- peek ps >>= peekCString
             xs <- parsePtrArray (n-1) (advancePtr ps 1)
             return ( x : xs )

xsRead :: String -> IO (Maybe String)
xsRead p = withXs $ \h -> xsReadH h p

xsWrite :: String -> String -> IO ()
xsWrite p v = withXs $ \h -> xsWriteH h p v

xsRm :: String -> IO ()
xsRm p = withXs $ \h -> xsRmH h p

xsWaitFor :: String -> IO Bool -> IO ()
xsWaitFor p pred = withXs $ \h -> xsWaitForH h p pred

xsDir :: String -> IO [String]
xsDir p = withXs $ \h -> xsDirH h p

-- uses global handle
withXs :: (Xs -> IO a) -> IO a
withXs f = f xsGlobal

---------------
-- PERMISSIONS
---------------
xsPERM_NONE = 0
xsPERM_READ = 1
xsPERM_WRITE = 2
xsPERM_ENOENT_OK = 4
xsPERM_OWNER = 8

data PermissionFlag = PermRead | PermWrite | PermEnoentOk | PermOwner
data Permission = Permission !Int [PermissionFlag]

unpackPerms :: CInt -> [PermissionFlag]
unpackPerms f | (f .&. xsPERM_READ) /= 0 = PermRead : unpackPerms (f .&. xsPERM_READ)
              | (f .&. xsPERM_WRITE) /= 0 = PermWrite : unpackPerms (f .&. xsPERM_WRITE)
              | (f .&. xsPERM_ENOENT_OK) /= 0 = PermEnoentOk : unpackPerms (f .&. xsPERM_ENOENT_OK)
              | (f .&. xsPERM_OWNER) /= 0 = PermOwner : unpackPerms (f .&. xsPERM_OWNER)
              | otherwise = []

packPerms :: [PermissionFlag] -> CInt
packPerms [] = 0
packPerms (PermRead : xs) = xsPERM_READ .|. packPerms xs
packPerms (PermWrite : xs) = xsPERM_WRITE .|. packPerms xs
packPerms (PermEnoentOk : xs) = xsPERM_ENOENT_OK .|. packPerms xs
packPerms (PermOwner : xs) = xsPERM_OWNER .|. packPerms xs

instance Storable Permission where
    alignment _ = alignment ( undefined :: CUInt )
    sizeOf    _ = #{size xs_permissions_t}
    peek p      = do id <- #{peek xs_permissions_t, id} p :: IO CUInt
                     flags <- #{peek xs_permissions_t, perms} p
                     return $ Permission (fromIntegral id) (unpackPerms flags)
    poke p v    = do let Permission id flags = v
                     #{poke xs_permissions_t, id} p id
                     #{poke xs_permissions_t, perms} p (packPerms flags)

xsGetPermissionsH :: Xs -> String -> IO [Permission]
xsGetPermissionsH xs path =
    withCString path $ \pathS ->
    alloca $ \num_ptr ->
        do perm_ptr <- c_xs_get_permissions (handle xs) 0 pathS num_ptr
           when (nullPtr == perm_ptr) $ error "c_xs_get_permissions failed"
           num <- fromIntegral <$> peek num_ptr
           perms <- peekArray num perm_ptr
           free perm_ptr
           return perms

xsSetPermissionsH :: Xs -> String -> [Permission] -> IO ()
xsSetPermissionsH xs path perms =
    withCString path $ \pathS ->
    withArrayLen perms $ \num_perms perm_ptr ->
        do rval <- c_xs_set_permissions (handle xs) 0 pathS perm_ptr (fromIntegral num_perms)
           when (rval == 0) $ error "c_xs_set_permissions failed"

xsGetPermissions path = withXs $ \h -> xsGetPermissionsH h path
xsSetPermissions path perms = withXs $ \h -> xsSetPermissionsH h path perms
