module Sample.Sushi where

import Prelude
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT, evalStateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as A
import Data.FoldableWithIndex (forWithIndex_)
import Data.String.CodeUnits as SCU
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Class (liftEffect)
import Europa.Terminal as Term
import Europa.TickerT as Tick
import Europa.Util as U

field :: { width :: Int, height :: Int } -> Array String
field { width, height } =
  map SCU.fromCharArray
    $ [ outerLane
      , lane
      , innerLane
      ]
    <> A.replicate (height - 6) body
    <> [ innerLane
      , lane
      , outerLane
      ]
  where
  outerLane = [ '+' ] <> A.replicate (width - 2) '-' <> [ '+' ]

  lane = [ '|' ] <> A.replicate (width - 2) ' ' <> [ '|' ]

  innerLane = [ '|', ' ', '+' ] <> A.replicate (width - 6) '-' <> [ '+', ' ', '|' ]

  body = [ '|', ' ', '|' ] <> A.replicate (width - 6) ' ' <> [ '|', ' ', '|' ]

idxToPos :: { width :: Int, height :: Int } -> Int -> { row :: Int, col :: Int }
idxToPos size@{ width, height } idx =
  if isTop idx then
    { col: idx + 2, row: 2 }
  else
    if isRight idx then
      { col: width - 1, row: idx - width + 5 }
    else
      if isBottom idx then
        { col: width * 2 + height - idx - 7, row: height - 1 }
      else
        if isLeft idx then
          { col: 2, row: width * 2 + height * 2 - idx - 10 }
        else
          idxToPos size (idx `mod` (width * 2 + height * 2 - 12))
  where
  isTop = between 0 (width - 3)

  isRight = between (width - 4) (width + height - 6)

  isBottom = between (width + height - 7) (width * 2 + height - 9)

  isLeft = between (width * 2 + height - 10) (width * 2 + height * 2 - 12)

sushi :: Aff Unit
sushi =
  Term.runTerminal
    $ Tick.runTickerT 16
    $ evalStateT go 0
  where
  go :: StateT Int (Tick.TickerT Term.Terminal) Unit
  go = do
    lift $ lift Term.hideCursor
    eol <- SCU.singleton <$> lift (liftEffect U.getEOL)
    forever do
      (Milliseconds d) <- lift Tick.getDeltaTime
      when (d > 250.0) do
        lift $ lift Term.clear
        size <- lift $ lift $ Term.getWindowSize
        lift $ lift $ Term.write { row: 1, col: 1 }
          $ A.intercalate eol (field size)
        n <- get
        forWithIndex_ (SCU.toCharArray "s u s h i") \i c -> do
          lift $ lift $ Term.writeChar (idxToPos size (i + n)) c
        modify_ (add 1)
        lift $ lift $ Term.redraw
        lift $ Tick.tick
