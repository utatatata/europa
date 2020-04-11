module Europa.Terminal
  ( Terminal
  , runTerminal
  , moveCursor
  , cursorTo
  , hideCursor
  , showCursor
  , setGraphics
  , getWindowSize
  , write
  , writeChar
  , clear
  , redraw
  ) where

import Prelude
import Ansi.Codes (GraphicsParam(..), EscapeCode(..), EraseParam(..), escapeCodeToString)
import Ansi.Output (withGraphics)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, evalStateT, gets, put)
import Data.List.NonEmpty (NonEmptyList)
import Data.Newtype (class Newtype, unwrap)
import Data.String as S
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Europa.Terminal.Internal as I

data TerminalState
  = TerminalState (NonEmptyList GraphicsParam)

type Internal a
  = (StateT TerminalState Effect) a

newtype Terminal a
  = Terminal (Internal a)

derive instance newtypeTerminal :: Newtype (Terminal a) _

derive newtype instance functorTerminal :: Functor Terminal

derive newtype instance applyTerminal :: Apply Terminal

derive newtype instance applicativeTerminal :: Applicative Terminal

derive newtype instance bindTerminal :: Bind Terminal

derive newtype instance monadTerminal :: Monad Terminal

derive newtype instance monadRecTerminal :: MonadRec Terminal

derive newtype instance moandStateTerminal :: MonadState TerminalState Terminal

derive newtype instance monadEffectTerminal :: MonadEffect Terminal

getGraphics :: Terminal (NonEmptyList GraphicsParam)
getGraphics = gets \(TerminalState g) -> g

runTerminal :: Terminal ~> Effect
runTerminal m = do
  I.cork
  a <- evalStateT (unwrap m) (TerminalState (pure Reset))
  I.uncork
  pure a

moveCursor :: { row :: Int, col :: Int } -> Terminal Unit
moveCursor { row, col } = liftEffect $ I.write $ escapeCodeToString (Down row) <> escapeCodeToString (Forward col)

cursorTo :: { row :: Int, col :: Int } -> Terminal Unit
cursorTo { row, col } = liftEffect $ I.write $ escapeCodeToString (Position row col)

withCursor :: Terminal ~> Terminal
withCursor m = do
  liftEffect $ I.write $ escapeCodeToString SavePosition
  a <- m
  liftEffect $ I.write $ escapeCodeToString RestorePosition
  pure a

hideCursor :: Terminal Unit
hideCursor = liftEffect $ I.write $ escapeCodeToString HideCursor

showCursor :: Terminal Unit
showCursor = liftEffect $ I.write $ escapeCodeToString ShowCursor

setGraphics :: NonEmptyList GraphicsParam -> Terminal Unit
setGraphics = put <<< TerminalState

getWindowSize :: Terminal { width :: Int, height :: Int }
getWindowSize = liftEffect I.getWindowSize

write :: { row :: Int, col :: Int } -> String -> Terminal Unit
write pos@{ row, col } s = do
  { width, height } <- getWindowSize
  let
    validStr = S.take (width - col) s
  when (between 1 height row && S.length validStr > 0) do
    withCursor do
      cursorTo pos
      g <- getGraphics
      liftEffect $ I.write $ withGraphics g s

writeChar :: { row :: Int, col :: Int } -> Char -> Terminal Unit
writeChar pos = write pos <<< SCU.singleton

clear :: Terminal Unit
clear = liftEffect $ I.write $ escapeCodeToString (EraseData Entire)

redraw :: Terminal Unit
redraw =
  liftEffect do
    I.uncork
    I.cork
