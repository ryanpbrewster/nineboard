import Html.App as App

import GameData exposing (Grid, emptyGrid)
import GameDisplay exposing (viewGrid)

main =
  App.beginnerProgram { model = emptyGrid, view = view, update = update }


type alias Model = Grid
type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model

    Decrement ->
      model

view = viewGrid
