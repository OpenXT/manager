--
-- Copyright (c) 2013 Citrix Systems, Inc.
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
module Main where

import Data.String
import Data.List
import Data.Maybe
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Control.Monad
import Control.Applicative
import qualified Control.Exception as E
import Text.Printf
import System.Exit
import System.Environment
import System.Posix.Syslog
import Cmd
import Parse
import Show

import Network.DBus
import Network.DBus.Actions (serializeSignature)
import Network.DBus.Introspect

import qualified DBusArgo as A

str = DBusString . PackedString . C.pack

call_ c n b = call c n b `E.catch` err where
  err :: DBusError -> IO a
  err x = do
    putStrLn $ printf "%s: %s" (errorName x) (describe $ errorBody x)
    exitWith (ExitFailure 1)
  describe xs = intercalate ", " $ map showDBus xs

introspect :: DBusConnection -> Location -> IO Object
introspect c loc =
  return . mkObject =<< (call_ c (fromString $ service loc) $
    DBusCall (fromString $ object loc) "Introspect" (Just "org.freedesktop.DBus.Introspectable") [])
  where
    mkObject r = case returnBody r of
      [ DBusString str ] ->
        case fromXml ( TL.fromChunks [TE.decodeUtf8 $ ustringToBS str] ) of
          Left err -> error err
          Right  v -> v
      _ -> error "bad introspection reply"

getProperty :: DBusConnection -> Location -> String -> IO String
getProperty c loc@(Location srv obj (Just intf)) property =
  return . rval =<< ( call_ c (fromString srv) $
    DBusCall (fromString obj) "Get" (Just "org.freedesktop.DBus.Properties") [ str intf, str property ] )
  where
    rval r = case returnBody r of
      [ v ] -> showDBus v
      _ -> error "getProperty: rpc returned invalid value"
getProperty c _ _ = error "interface not given"

setProperty :: DBusConnection -> Location -> String -> DBusValue -> IO ()
setProperty c loc@(Location srv obj (Just intf)) property v = do
  call_ c (fromString srv) $
    DBusCall (fromString obj) "Set" (Just "org.freedesktop.DBus.Properties") [ str intf, str property, DBusVariant v ]
  return ()
setProperty c _ _ _ = error "interface not given"

getAllProperties :: DBusConnection -> Location -> IO [(DBusValue, DBusValue)]
getAllProperties c loc@(Location srv obj (Just intf)) =
  return . rval =<< ( call_ c (fromString srv) $
    DBusCall (fromString obj) "GetAll" (Just "org.freedesktop.DBus.Properties") [ str intf ] )
  where
    rval r = case returnBody r of
      [ DBusArray _ xs ] -> map conv xs
      _ -> error "getAllProperties: rpc returned invalid value"
    conv (DBusDict k v) = (k,v)
    conv _ = error "unexpected item"
getAllProperties c _ = error "interface not given"

withConnection :: Flags -> (DBusConnection -> IO a) -> IO a
withConnection flags f =
  f =<< establish bus authenticateWithRealUID
  where
    bus | domid flags >= 0 = A.domainSystemBus (domid flags)
        | session flags    = busGetSession
        | otherwise        = busGetSystem
    domainBus domid = A.domainSystemBus domid

findInterface :: Object -> Text -> Maybe Interface
findInterface obj name = find test (objInterfaces obj) where
  test i = name == intfName i

findMethod :: Interface -> Text -> Maybe Method
findMethod intf name = find test (intfMethods intf) where
  test m = name == methName m

findProperty :: Interface -> Text -> Maybe Property
findProperty intf name = find test (intfProperties intf) where
  test p = name == propName p

findPropertyInterfaces :: Object -> Text -> [Interface]
findPropertyInterfaces obj name = filter test (objInterfaces obj) where
  test i = name `elem` map propName (intfProperties i)

findMethodInterfaces :: Object -> Text -> [Interface]
findMethodInterfaces obj name = filter test (objInterfaces obj) where
  test i = name `elem` map methName (intfMethods i)

addPropertyInterface obj name loc@(Location _ _ (Just _)) = loc
addPropertyInterface obj name loc = case findPropertyInterfaces obj name of
  (i:_) -> loc { interface = Just (TL.unpack $ intfName i) }
  _     -> loc

addMethodInterface obj name loc@(Location _ _ (Just _)) = loc
addMethodInterface obj name loc = case findMethodInterfaces obj name of
  (i:_) -> loc { interface = Just (TL.unpack $ intfName i) }
  _     -> loc

listInterfaceMethods :: Interface -> IO ()
listInterfaceMethods i = mapM_ (putStrLn . replace '_' '-' . TL.unpack . methName) (intfMethods i)

listInterfaceProperties :: Interface -> IO ()
listInterfaceProperties i = mapM_ (putStrLn . TL.unpack . propName) (intfProperties i)

cmdListInterfaces :: DBusConnection -> Location -> IO ()
cmdListInterfaces c loc =
  do o <- introspect c loc
     mapM_ (putStrLn . TL.unpack . intfName) (objInterfaces o)

cmdListMethods :: DBusConnection -> Location -> IO ()
cmdListMethods c loc =
  do o <- introspect c loc
     let intf = interface loc >>= findInterface o . TL.pack
     case intf of
       Just i -> listInterfaceMethods i
       _      -> mapM_ listInterfaceMethods (objInterfaces o)

cmdListProperties :: DBusConnection -> Location -> IO ()
cmdListProperties c loc =
  do o <- introspect c loc
     let intf = interface loc >>= findInterface o . TL.pack
     case intf of
       Just i -> listInterfaceProperties i
       _      -> mapM_ listInterfaceProperties (objInterfaces o)

cmdGet :: DBusConnection -> Location -> String -> IO ()
cmdGet c loc p =
  do loc' <- case interface loc of
       Just _ -> return loc
       _      -> introspect c loc >>= \obj -> return $ addPropertyInterface obj (TL.pack p) loc
     case interface loc' of
       Nothing -> error $ printf "property %s not found in any of the interfaces" p
       Just _  -> putStrLn =<< getProperty c loc' p

cmdGetall :: DBusConnection -> Location -> IO ()
cmdGetall c loc =
  case interface loc of
    Nothing -> introspect c loc >>= \obj -> mapM_ (getallIntf c) (objInterfaces obj)
    Just i  -> dump i
  where
    getallIntf c i = cmdGetall c $ loc { interface = Just (TL.unpack $ intfName i) }
    dump intf | intf == "org.freedesktop.DBus.Properties" = return ()
    dump intf = do
      xs <- getAllProperties c loc
      when (not $ null xs) $ do
        putStrLn intf
        putStrLn "{"
        mapM_ putEntry xs
        putStrLn "}"
    putEntry (k,v) =
      putStrLn $ printf "  %-35s = %s" ("\"" ++ showDBus k ++ "\"") (showDBus v)

cmdSet :: DBusConnection -> Location -> PropertyName -> Value -> IO ()
cmdSet c loc p arg =
  do obj <- introspect c loc
     loc' <- case interface loc of
       Just _ -> return loc
       _      -> return $ addPropertyInterface obj (TL.pack p) loc
     case interface loc' of
       Nothing   -> error $ printf "property %s not found in any of the interfaces" p
       Just intf -> let Just intf' = findInterface obj (TL.pack intf) in go loc' intf'
  where
    name' = TL.pack p
    go loc' intf = do
      let Just prop = findProperty intf name'
      value <- parseArg (1, propType prop, arg)
      setProperty c loc' p value

cmdCall :: DBusConnection -> Location -> MethodName -> [Value] -> IO ()
cmdCall c loc name args =
  do obj <- introspect c loc
     loc' <- case interface loc of
       Just _ -> return loc
       _      -> return $ addMethodInterface obj name' loc
     case interface loc' of
       Nothing   -> error $ printf "method %s not found in any of the interfaces" name
       Just intf -> case findInterface obj (TL.pack intf) of
                         Nothing -> error $ printf "interface %s not found" intf
                         Just intf' -> go intf'
  where
    name' = TL.pack name
    go intf = do
      case findMethod intf name' of
        Nothing -> error $ printf "method %s.%s not found" (TL.unpack $ intfName intf) name
        Just m  -> let exp_len = length (methInArgs m) in do
          when (exp_len /= length args) $
            error $ printf "method %s expects %d args, but %d were given" name exp_len (length args)
          values <- mapM parseArg $ zip3 [1..] (map argType $ methInArgs m) args
          output =<< (
            call_ c (fromString $ service loc) $
              DBusCall
                (fromString $ object loc)
                name
                (Just $ TL.unpack (intfName intf))
                values
            )
      where
        output = mapM_ (putStrLn . showDBus) . returnBody

cmdIntrospect :: DBusConnection -> Location -> IO ()
cmdIntrospect c loc =
  do obj <- introspect c loc
     putStrLn (renderObj obj)
     putStrLn ""

renderObj :: Object -> String
renderObj obj =
  intercalate "\n\n" . map renderInterface $ objInterfaces obj

renderInterface :: Interface -> String
renderInterface i
  = "/------- " ++ (TL.unpack $ intfName i) ++ "\n"
    ++ intercalate "\n" (   map renderProperty (intfProperties i)
                         ++ map renderMethod   (intfMethods i) )
  where
    renderProperty p
     = "|     -  " ++ printf "%-28s %s:%s" (TL.unpack $ propName p) (renderAccess $ propAccess p) (renderType $ propType p)
    renderMethod m
     = "|     +  " ++ printf "%-28s %s" (replace '_' '-' $ TL.unpack $ methName m) (renderArgs m :: String)
    renderArgs m
      = printf "( %s )" $ intercalate ", " $
             map renderIArg  (methInArgs m)
          ++ map renderOArg (methOutArgs m)
    renderIArg a
      = printf "%s:%s" name typ
        where name = fromMaybe "" $ TL.unpack `fmap` argName a
              typ  = renderType (argType a)
    renderOArg a = "OUT " ++ renderIArg a
    renderType t = UTF8.toString $ serializeSignature [t]
    renderAccess Read = "read"
    renderAccess Write = "write"
    renderAccess ReadWrite = "readwrite"


parseArg :: (Int,SignatureElem,String) -> IO DBusValue
parseArg (index,arg_type,str) =
  case parseDBus str arg_type of
    Nothing -> error $ printf "error parsing argument %d" ( index :: Int )
    Just v  -> return v

exec (Cmd flags cmd)
  = case cmd of
      CmdListInterfaces loc -> withConnection flags $ \c -> cmdListInterfaces c loc
      CmdListMethods loc -> withConnection flags $ \c -> cmdListMethods c loc
      CmdListProperties loc -> withConnection flags $ \c -> cmdListProperties c loc
      CmdGet loc p -> withConnection flags $ \c -> cmdGet c loc p
      CmdGetAll loc -> withConnection flags $ \c -> cmdGetall c loc
      CmdSet loc (p,v) -> withConnection flags $ \c -> cmdSet c loc p v
      CmdCall loc m args -> withConnection flags $ \c -> cmdCall c loc m args
      CmdIntrospect loc -> withConnection flags $ \c -> cmdIntrospect c loc
      CmdHelp -> putStrLn usageStr

usage =
  do putStrLn usageStr
     exitWith (ExitFailure 1)

usageStr = unlines [
  "Usage: xec [options] [get/set/getall] <method/property> arg1 arg2 ... argN"
 ,""
 ,"examples:"
 ,"  * to introspect all interfaces implemented by object"
 ,"   xec -s <service-name> -o <object-name>"
 ,"  * to introspect specific interface implemented by object"
 ,"   xec -s <service-name> -o <object-name> -i <interface-name>"
 ,"  * to get a property value"
 ,"   xec -s <service-name> -o <object-name> -i <interface-name> -g <property-name>"
 ,"   OR"
 ,"   xec -s <service-name> -o <object-name> -i <interface-name> get <property-name>"
 ,"  * to get all properties with values"
 ,"   xec -s <service-name> -o <object-name> -i <interface-name> -g"
 ,"   OR"
 ,"   xec -s <service-name> -o <object-name> -i <interface-name> getall"
 ,"  * to set property value"
 ,"   xec -s <service-name> -o <object-name> -i <interface-name> -x <property-name> <property-value>"
 ,"  * to call a method"
 ,"   xec -s <service-name> -o <object-name> -i <interface-name> <method-name> arguments"
 ,""
 ,"Options:"
 ,"    -q, --query                      query mode"
 ,"    -t, --target-domid DOMID         select target domain"
 ,"    -s, --service SERVICE            select service"
 ,"    -o, --object OBJECT              select object"
 ,"    -i, --interface INTERFACE        select interface"
 ,"    -g, --get PROPERTY               get value(s) of property(ies)"
 ,"    -x, --set PROPERTY VALUE         set value of property"
 ,"        --session                    use session bus"
 ,"        --list-interfaces"
 ,"                                     list interfaces"
 ,"        --list-methods"
 ,"                                     list methods"
 ,"        --list-properties"
 ,"                                     list properties"
 ,"    -h, --help                       display this message"
 ]

main = do
  args <- getArgs
  case getCmd args of
    Nothing  -> usage
    Just cmd@(Cmd flags _) ->
      do when (not $ query flags) $
           withSyslog "xec" [] USER $ syslog Info ( "invoked with: " ++ intercalate " " args )
         exec cmd
