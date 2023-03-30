-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright (c) 2012 Citrix Systems, Inc.
-- Copyright (c) 2018 Jason Andryuk

{-# LANGUAGE PatternGuards #-}

-- description: Change kernel to grub-xen-pvh/pv64

module Migrations.M_41 (migration) where

import UpgradeEngine
import Text.Printf

migration = Migration {
              sourceVersion = 41
            , targetVersion = 42
            , actions = act
            }

act :: IO ()
act = updateGrub

updateGrub = xformVmJSON xform where
  xform tree = case (jsGet "/type" tree, jsGet "/config/virt-type" tree, jsGet "/config/disk/0/path" tree) of
    (Just t, Just v, Just p) -> modify (jsUnboxString t) (jsUnboxString v) (jsUnboxString p) tree
    _ -> tree
    where
      modifyPv = jsRm "/config/kernel-extract" .
                 jsSet "/config/kernel" (jsBoxString "/usr/lib/xen/boot/grub-xen-pv64")
      modifyPvh = jsRm "/config/kernel-extract" .
                  jsSet "/config/kernel" (jsBoxString "/usr/lib/xen/boot/grub-xen-pvh")
      modify "ndvm" "pv" "/storage/ndvm/ndvm.vhd" tree = modifyPv tree
      modify "syncvm" "pv" "/storage/syncvm/syncvm.vhd" tree = modifyPv tree
      modify "syncvm" "pvh" "/storage/syncvm/syncvm.vhd" tree = modifyPvh tree
      modify t v p tree = tree
