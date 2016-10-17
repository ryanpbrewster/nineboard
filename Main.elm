import Html.App as App
import Html

import Data
import Display
import Update

main =
  App.beginnerProgram 
    { model = Data.initialState
    , view = Display.view
    , update = Update.update
    }
