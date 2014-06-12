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

-- OVF environment generation
module OVF.Env
       (
         VmEnv (..)
       , env
       , envStr
       ) where

import Text.XML.HXT.Core
import OVF.Model
import qualified Vm.ProductProperty as Vm

data VmEnv
   = VmEnv SystemID [Vm.ProductProperty]

envStr :: LA () XmlTree -> String
envStr e = f strs where
  strs    = runLA (root [] [e] >>> writeDocumentToString [withOutputEncoding utf8, withXmlPi yes, withIndent yes]) ()
  f []    = error "xml serialise error"
  f (x:_) = x

env :: ArrowXml a => VmEnv -> [VmEnv] -> a () XmlTree
env (VmEnv (SystemID sysID) properties) siblings =
  mkelem "Environment" [
      sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
    , sattr "xmlns:ovfenv" "http://schemas.dmtf.org/ovf/environment/1"
    , sattr "xmlns" "http://schemas.dmtf.org/ovf/environment/1"
    , sattr "ovfenv:id" sysID
    ]
    (
      [ selem "PlatformSection" [ selem "Kind" [ txt "XenClient XT" ] ]  
      , selem "PropertySection" (map property properties)
      ] ++ map entity siblings
    )
  
entity :: ArrowXml a => VmEnv -> a () XmlTree
entity (VmEnv (SystemID sysID) properties) =
  mkelem "Entity" [ sattr "ovfenv:id" sysID ]
  [ selem "PropertySection" (map property properties) ]

property :: ArrowXml a => Vm.ProductProperty -> a () XmlTree
property p =
  mkelem "Property" [ sattr "ovfenv:key" key, sattr "ovfenv:value" value ] [ ]
  where
    key   = Vm.strPPUniqueID (Vm.ppUniqueID p)
    value = Vm.ppValue p
