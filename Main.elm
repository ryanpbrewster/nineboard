import Html

import Data
import Display
import Update

main =
  Html.beginnerProgram 
    { model = Data.initialState
    , view = Display.view
    , update = Update.update
    }
