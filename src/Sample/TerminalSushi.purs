module Sample.TerminalSushi where

import Prelude
import Data.Array as A
import Data.FoldableWithIndex (forWithIndex_)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Europa.Terminal as T
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

terminalSushi :: Effect Unit
terminalSushi = void $ setTimeout 0 (go 0)
  where
  go :: Int -> Effect Unit
  go n =
    T.runTerminal do
      eol <- SCU.singleton <$> liftEffect U.getEOL
      T.hideCursor
      T.clear
      size <- T.getWindowSize
      T.writeWithoutWrap { row: 1, col: 1 } $ A.intercalate eol (field size)
      forWithIndex_ (SCU.toCharArray "s u s h i") \i c -> do
        T.writeChar (idxToPos size (i + n)) c
      T.redraw
      void $ liftEffect $ setTimeout 125 (go (n + 1))
