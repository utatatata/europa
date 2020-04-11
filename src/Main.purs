module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Sample.Sushi (sushi)

main :: Effect Unit
main = do
  launchAff_ sushi
