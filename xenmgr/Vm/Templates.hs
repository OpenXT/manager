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

{-# LANGUAGE PatternGuards #-}
module Vm.Templates
    ( exportTemplate
    , getNewVmTemplate
    , getServiceVmTemplate
    , getChildServiceVmTemplate
    , getAnyVmTemplate
    , nullTemplate
    , templateFromString
    , enumTemplateTags
    , enumUiTemplates
    , enumServiceVmTags
    , enumChildServiceVmTags
    , getUuidInTemplate
    , getSlotInTemplate
    , applyChildTemplateToVm
    , mergeTemplates
    ) where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Data.String

import System.IO
import System.FilePath
import Tools.JSONTrees
import Tools.Misc
import Tools.File
import Tools.Log
import Tools.Text

import Vm.Types
import XenMgr.Db
import XenMgr.Config
import XenMgr.Rpc
import Paths_xenmgr

data ConfigTemplate = ConfigTemplate JSValue

ndvmDefaultMode = "hvm"

getNdvmMode :: Rpc String
getNdvmMode =
    do mode <- dbReadWithDefault ndvmDefaultMode "/xenmgr/ndvm-mode"
       return $ mode

getUsbvmEnable :: Rpc Bool
getUsbvmEnable =
    do run <- dbReadWithDefault False "/xenmgr/usbvm-enable"
       return $ run

readJSONFile :: FilePath -> IO JSValue
readJSONFile path = do
  contents <- readFile path
  case decodeStrict $ contents of
    Error msg -> error $ "malformed JSON file: " ++ msg
    Ok json   -> return json

getTemplateDir :: IO FilePath
getTemplateDir =
    do flavour <- appGetPlatformFlavour
       getDataFileName ("templates" </> flavour)

getTemplateFile :: String -> IO FilePath
getTemplateFile name =
    do dir <- getTemplateDir
       return $ dir </> name

getNewVmTemplate :: IO ConfigTemplate
getNewVmTemplate = getAnyVmTemplate "new-vm"

getServiceVmTemplate :: String -> IO ConfigTemplate
getServiceVmTemplate tag = getAnyVmTemplate ("service-" ++ tag)

getChildServiceVmTemplate :: String -> IO ConfigTemplate
getChildServiceVmTemplate tag = getAnyVmTemplate ("child-" ++ tag)

getAnyVmTemplate :: String -> IO ConfigTemplate
getAnyVmTemplate tag =
    do fname <- getTemplateFile tag
       ConfigTemplate <$> readJSONFile fname

nullTemplate :: ConfigTemplate
nullTemplate = ConfigTemplate . JSObject . toJSObject $ []

templateFromString :: String -> ConfigTemplate
templateFromString s = case strip s of
  "" -> nullTemplate
  s  -> case decode s of
             Error msg -> error $ "malformed JSON: " ++ msg
             Ok json -> ConfigTemplate json

enumTemplateTags :: IO [String]
enumTemplateTags =
    do template_dir <- getTemplateDir
       tags <- catMaybes . map get_tag <$> filesInDir template_dir
       return $ tags
    where
      get_tag f = case takeBaseName f of
                    's':'e':'r':'v':'i':'c':'e':'-':tag -> Nothing
                    tag -> Just tag

-- UI selectable templates
enumUiTemplates :: IO [(String, String)]
enumUiTemplates = do
  tags <- enumTemplateTags
  templates <- zip tags <$> mapM getAnyVmTemplate tags
  return $ map mk $ filter isUI templates
  where
    isUI (tag, template) = getStringInTemplate template "/ui-selectable" == Just "true"
    mk (tag, template) = (tag, descr) where
      descr = case getStringInTemplate template "/description" of
        Nothing -> ""
        Just d  -> d

enumServiceVmTags :: Rpc [String]
enumServiceVmTags =
    do tags <- catMaybes . map get_tag <$> template_files
       ndvm_mode <- getNdvmMode
       usbvm_enable <- getUsbvmEnable
       info $ "ndvm mode " ++ ndvm_mode ++ " tags: " ++ ( intercalate " " tags )
       return $ filter ( filt_ndvm ndvm_mode usbvm_enable ) tags
    where
      template_files = liftIO $ do template_dir <- getTemplateDir
                                   filesInDir template_dir
      get_tag f = case takeBaseName f of
                    's':'e':'r':'v':'i':'c':'e':'-':tag -> Just tag
                    _ -> Nothing
      filt_ndvm ndvm_mode usbvm_enable f = case f of
                                'n':'d':'v':'m':'-':mode -> mode == ndvm_mode
                                "usbvm" -> usbvm_enable
                                _ -> True

enumChildServiceVmTags :: IO [String]
enumChildServiceVmTags =
    do template_dir <- getTemplateDir
       tags <- catMaybes . map get_tag <$> filesInDir template_dir
       return $ tags
    where
      get_tag f = case takeBaseName f of
                    'c':'h':'i':'l':'d':'-':tag -> Just tag
                    _ -> Nothing

data StoredKey = StoredKey String String
--
-- Write a configuration template to database
--
exportTemplate :: MonadRpc e m => Maybe Uuid -> ConfigTemplate -> m Uuid
exportTemplate maybe_uuid (ConfigTemplate json_) =
    let uuid = case maybe_uuid of
                 Just uuid -> uuid
                 Nothing   -> case jsGet "/uuid" json_ of
                                Just s -> fromString . jsUnboxString $ s
                                Nothing  -> error "uuid required, but template does not specify uuid"
        json = jsSet "/uuid" (jsBoxString . show $ uuid) json_
    in do csums <- get_disk_csums uuid
          let json' = foldr set json csums
              set (StoredKey k v) j = jsSet k (jsBoxString v) j
              str = encode json'
          dbInject (vmDbPath uuid) str    
          return uuid
    where
      get_disk_csums uuid = do
        paths  <- map (\p -> "/config/disk/" ++ p ++ "/sha1sum") <$> dbList disk_path
        values <- mapM (\p -> dbMaybeRead (vmDbPath uuid ++ "/" ++ p)) paths
        return $ foldr conv [] $ zip paths values
        where
          disk_path = vmDbPath uuid ++ "/config/disk"
          conv (_, Nothing) acc = acc
          conv (p, Just v ) acc = StoredKey p v : acc

-- overrides the parent configuration with data found in
-- override section within template
applyChildTemplateToVm :: MonadRpc e m => (ConfigTemplate,Uuid) -> Uuid -> m Bool
applyChildTemplateToVm (ConfigTemplate t, child_uuid) vm_uuid =
    case jsGet "/parent-override" t of
      Nothing -> return True -- no override sect
      Just ov ->
        let ov_t = substituteVars
                   (ConfigTemplate ov)
                   (SubstVars { subParentUuid = vm_uuid
                              , subUuid = child_uuid } )
        in
          templatedModifyVm (\vm_t -> mergeTemplates vm_t ov_t) vm_uuid

-- merge two templates (values in template 2 take precedence)
mergeTemplates :: ConfigTemplate -> ConfigTemplate -> ConfigTemplate
mergeTemplates (ConfigTemplate a) (ConfigTemplate b)
  = ConfigTemplate (merge a b)
    where
      -- (o1 - o2) + (o2 - o1) + (o2 x o1) where 'x' - intersection retaining first operand values
      merge (JSObject a) (JSObject b) = merge_objs a b
      merge _ x = x
      
      merge_objs a b =
          let tuples_a = fromJSObject a
              tuples_b = fromJSObject b in
          JSObject $ toJSObject $
                       (tuples_a `minusT`     tuples_b) `plusT`
                       (tuples_b `minusT`     tuples_a) `plusT`
                       (tuples_a `intersectT` tuples_b)

      plusT            = (++)
      a `minusT` b     = filter (not . contained_in b) a
      a `intersectT` b = foldr f [] a where
        f (k, v) xs
          | Just (_,v') <- find ((== k). fst) b = (k, merge v v') : xs
          | otherwise                           = xs
      contained_in b x = fst x `elem` map fst b

-- map f x over atomic values in template
mapTemplate :: (JSValue -> JSValue) -> ConfigTemplate -> ConfigTemplate
mapTemplate f (ConfigTemplate t) = ConfigTemplate (aux t) where
    aux (JSArray xs) = JSArray  $ map aux xs
    aux (JSObject o) = JSObject . toJSObject $ map g (fromJSObject o) where g (name,v) = (name, aux v)
    aux other = f other

data SubstVars = SubstVars { subParentUuid :: Uuid
                           , subUuid :: Uuid }

substituteVars :: ConfigTemplate -> SubstVars -> ConfigTemplate
substituteVars t sv =
    mapTemplate f t where
        f (JSString s) = JSString . toJSString $
                         "${PARENT_UUID}" `replace` (show $ subParentUuid sv) $
                         "${UUID}" `replace` (show $ subUuid sv) $
                         fromJSString s
        f x = x


readVmAsTemplate :: MonadRpc e m => Uuid -> m (Maybe ConfigTemplate)
readVmAsTemplate uuid =
    fmap ConfigTemplate <$> dbDump (vmDbPath uuid)

templatedModifyVm :: MonadRpc e m => (ConfigTemplate -> ConfigTemplate) -> Uuid -> m Bool
templatedModifyVm f uuid =
    do modified <- fmap f <$> readVmAsTemplate uuid
       case modified of
         Nothing -> return False
         Just t  -> exportTemplate (Just uuid) t >> return True

getUuidInTemplate :: ConfigTemplate -> Maybe Uuid
getUuidInTemplate (ConfigTemplate json) =
    case jsGet "/uuid" json of
      Just s  -> Just . fromString . jsUnboxString $ s
      Nothing -> Nothing

getSlotInTemplate :: ConfigTemplate -> Maybe Int
getSlotInTemplate t = join . fmap maybeRead $ (getStringInTemplate t "/slot")

getStringInTemplate :: ConfigTemplate -> String -> Maybe String
getStringInTemplate (ConfigTemplate json) path =
  case jsGet path json of
    Just s -> Just . jsUnboxString $ s
    Nothing-> Nothing

-- A path to database from given VM
vmDbPath :: Uuid -> String
vmDbPath uuid = "/vm/" ++ show uuid
