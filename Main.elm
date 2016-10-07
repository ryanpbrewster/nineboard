import Html.App as App
import Html

import GameData
import GameDisplay
import GameInput

import Debug

main =
  App.beginnerProgram { 
    model = { 
      grid = GameData.emptyGrid,
      player = GameData.User
    }, 
    view = view,
    update = update 
  }


type alias Model = { grid: GameData.Grid, player: GameData.Player }

update : GameInput.Input -> Model -> Model
update msg model =
  let updatedModel = case msg of
    GameInput.Click pos -> model
  in Debug.log (toString msg) updatedModel

view : Model -> Html.Html GameInput.Input
view state = GameDisplay.viewGrid state.grid
