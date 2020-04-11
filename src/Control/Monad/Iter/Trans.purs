module Control.Monad.Iter.Trans where

import Prelude
import Control.Monad.Free.Trans (FreeT, runFreeT)
import Control.Monad.Free.Trans.Extra (wrapFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Identity (Identity)
import Data.Newtype (unwrap)

type IterT
  = FreeT Identity

runIterT :: forall m. Monad m => MonadRec m => IterT m ~> m
runIterT = runFreeT (pure <<< unwrap)

delay :: forall m a. Monad m => IterT m a -> IterT m a
delay = wrapFreeT <<< pure
