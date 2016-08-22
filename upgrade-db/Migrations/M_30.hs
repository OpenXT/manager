--
-- Copyright (c) 2016 Assured Information Security, Inc. <lejosnej@ainfosec.com>
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

-- description: add policy bit to deny USB mouse assignment to guests
-- date: 08/22/2016

module Migrations.M_30 (migration) where

import UpgradeEngine

migration = Migration {
              sourceVersion = 30
            , targetVersion = 31
            , actions = act
            }

-- The following uses ".", so it should be read from bottom to top
act :: IO ()
act = xformPrimaryJSON $ policy
  where
    policy = jsSet "/usb-rules/9901/device/mouse"    (jsBoxString "1") .
             jsSet "/usb-rules/9901/command"         (jsBoxString "deny") .
             jsSet "/usb-rules/9901/description"     (jsBoxString "Deny mouse passthrough to all VMs")
