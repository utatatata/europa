module Main where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Europa.TickerT (runTickerT, tick)
import Sample.TerminalSushi (terminalSushi)

main :: Effect Unit
main = do
  -- terminalSushi
  launchAff_
    $ runTickerT 2 do
        forever do
          liftEffect $ log "loop!"
          tick
