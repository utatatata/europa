module Control.Monad.Free.Trans.Extra where

import Prelude
import Control.Monad.Free.Trans (FreeT, liftFreeT)

-- `MonadFree f (IterT m)` is a ophan instance, wrapFree can't be used.
wrapFreeT :: forall a m f. Functor f => Monad m => f (FreeT f m a) -> FreeT f m a
wrapFreeT = join <<< liftFreeT
