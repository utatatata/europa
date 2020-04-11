module Europa.Util
  ( size
  , vec
  , module Exports
  ) where

import Europa.Terminal.Internal (getEOL) as Exports

vec :: Int -> Int -> { x :: Int, y :: Int }
vec x y = { x, y }

size :: Int -> Int -> { width :: Int, height :: Int }
size width height = { width, height }
