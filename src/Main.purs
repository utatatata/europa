module Main where

import Prelude
import Effect (Effect)
import Sample.TerminalSushi (terminalSushi)

main :: Effect Unit
main = do
  terminalSushi
