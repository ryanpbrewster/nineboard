module GameUpdate exposing (..)

import GameData exposing (..)
import GameInput exposing (..)

handleClick : Position -> Player -> Grid -> Result String Grid
handleClick pos player grid =
  Ok (setCell pos (Filled player) grid)
