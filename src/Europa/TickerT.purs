module Europa.TickerT where

import Prelude
import Control.Monad.Free.Trans (runFreeT)
import Control.Monad.Iter.Trans (IterT, delay)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, evalStateT, gets, modify_)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.DateTime (DateTime, diff)
import Data.DateTime.Extra (getNow)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

data TickerReader
  = TickerReader { fps :: FPS, startTime :: DateTime }

data TickerState
  = TickerState { lastTickedTime :: DateTime }

type InternalT m
  = ReaderT TickerReader (StateT TickerState m)

newtype TickerT m a
  = TickerT (IterT (InternalT m) a)

type FPS
  = Int

runTickerT :: forall m. MonadRec m => MonadAff m => FPS -> TickerT m ~> m
runTickerT fps m = do
  now <- liftEffect getNow
  a <- evalStateT (runReaderT (runFreeT go (unwrap m)) (TickerReader { fps: fps, startTime: now })) (TickerState { lastTickedTime: now })
  pure a
  where
  go :: forall a. Identity (IterT (InternalT m) a) -> (InternalT m) (IterT (InternalT m) a)
  go (Identity a) = do
    now <- liftEffect getNow
    TickerReader { fps } <- ask
    liftAff $ Aff.delay $ Milliseconds $ 1000.0 / toNumber fps
    modify_ \(TickerState s) -> TickerState $ s { lastTickedTime = now }
    pure a

derive instance newtypeTickerT :: Newtype (TickerT m a) _

derive newtype instance functorTickerT :: Functor m => Functor (TickerT m)

derive newtype instance applyTickerT :: Monad m => Apply (TickerT m)

derive newtype instance applicativeTickerT :: Monad m => Applicative (TickerT m)

derive newtype instance bindTickerT :: Monad m => Bind (TickerT m)

derive newtype instance monadTickerT :: Monad m => Monad (TickerT m)

derive newtype instance monadRecTickerT :: MonadRec m => MonadRec (TickerT m)

derive newtype instance monadEffectTickerT :: MonadEffect m => MonadEffect (TickerT m)

derive newtype instance monadAffTickerT :: MonadAff m => MonadAff (TickerT m)

derive newtype instance moandAskTickerT :: Monad m => MonadAsk TickerReader (TickerT m)

derive newtype instance monadStateTickerT :: Monad m => MonadState TickerState (TickerT m)

instance monadTransTickerT :: MonadTrans TickerT where
  lift = wrap <<< lift <<< lift <<< lift

class
  MonadEffect m <= MonadTicker m where
  tick :: m Unit
  getFPS :: m FPS
  getLastTickedTime :: m DateTime
  getStartTime :: m DateTime

instance monadTickerTickerT :: MonadEffect m => MonadTicker (TickerT m) where
  tick = wrap <<< delay $ pure unit
  getFPS = asks \(TickerReader { fps }) -> fps
  getLastTickedTime = gets \(TickerState { lastTickedTime }) -> lastTickedTime
  getStartTime = asks \(TickerReader { startTime }) -> startTime

getDeltaTime :: forall m. MonadTicker m => m Milliseconds
getDeltaTime = do
  now <- liftEffect getNow
  l <- getLastTickedTime
  pure (now `diff` l)
