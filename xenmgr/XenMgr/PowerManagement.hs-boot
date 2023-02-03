module XenMgr.PowerManagement where

import XenMgr.Rpc
import XenMgr.Db
import XenMgr.XM

data PMAction = ActionSleep
              | ActionHibernate
              | ActionShutdown
              | ActionForcedShutdown
              | ActionIdleShutdown
              | ActionReboot
              | ActionNothing
              | ActionInvalid

instance Show PMAction
instance Eq PMAction
instance Marshall PMAction

executePmAction :: PMAction -> XM ()
hostWhenIdle :: (MonadRpc e m) => m () ->m ()
