import Browser
import Html

import Data
import Display
import Update

main =
  Browser.sandbox
    { init = Data.initialState
    , view = Display.view
    , update = Update.update
    }
