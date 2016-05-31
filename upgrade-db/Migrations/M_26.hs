--
-- Copyright (c) 2015 Assured Information Security, Inc. <lejosnej@ainfosec.com>
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

{-# LANGUAGE PatternGuards #-}

-- description: add default USB policy
-- date: 07/30/2015

module Migrations.M_26 (migration) where

import UpgradeEngine

migration = Migration {
              sourceVersion = 26
            , targetVersion = 27
            , actions = act
            }

-- The following uses ".", so it should be read from bottom to top
act :: IO ()
act = xformPrimaryJSON $ policy
  where
    policy = jsSet "/usb-rules/9999/command"         (jsBoxString "allow") .
             jsSet "/usb-rules/9999/description"     (jsBoxString "Allow everything else") .
             jsSet "/usb-rules/9901/device/mouse"    (jsBoxString "1") .
             jsSet "/usb-rules/9901/command"         (jsBoxString "deny") .
             jsSet "/usb-rules/9901/description"     (jsBoxString "Deny mouse passthrough to all VMs") .
             jsSet "/usb-rules/9900/device/keyboard" (jsBoxString "1") .
             jsSet "/usb-rules/9900/command"         (jsBoxString "deny") .
             jsSet "/usb-rules/9900/description"     (jsBoxString "Deny keyboard passthrough to all VMs")
