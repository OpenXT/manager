module Vm.Queries
   (getVmGpu,
    getVms
   ) where

import Vm.Uuid
import XenMgr.Rpc

getVmGpu :: MonadRpc e m => Uuid -> m String

getVms :: (MonadRpc e m) => m [Uuid]
