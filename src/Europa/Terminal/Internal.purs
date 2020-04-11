module Europa.Terminal.Internal where

import Prelude
import Effect (Effect)

foreign import getEOL :: Effect Char

foreign import rows :: Effect Int

foreign import columns :: Effect Int

getWindowSize :: Effect { width :: Int, height :: Int }
getWindowSize = { width: _, height: _ } <$> columns <*> rows

foreign import write :: String -> Effect Unit

foreign import cork :: Effect Unit

foreign import uncork :: Effect Unit
