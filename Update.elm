module Update exposing (..)

import Data exposing (..)
import Input exposing (..)
import Maybe exposing (..)
import Array as A

update : Input -> GameState -> GameState
update msg state =
  case msg of
    Click pos -> handleClick pos state

handleClick : Position -> GameState -> GameState
handleClick pos state =
  { state
    | grid = setCell pos (Filled state.currentPlayer) state.grid
    , currentPlayer = otherPlayer state.currentPlayer
    }

setCell : Position -> CellValue -> Grid -> Grid
setCell pos value grid =
  withDefault grid <|
    extractBoard pos.board grid `andThen` \board ->
      extractCell pos.cell board `andThen` \cell ->
        let
            newGrid = insertBoard (insertCell { cell | value = value } board) grid
            nextActive = {i = pos.cell.r, j = pos.cell.c}
        in 
            extractBoard nextActive newGrid `andThen` \nextBoard ->
              Just <| case nextBoard.value of
                WonBoard _ -> activateEntireGrid newGrid
                Cells _ _ -> activateBoard nextActive newGrid

setActive : IsActive -> Board -> Board
setActive isActive board =
  case board.value of
    WonBoard _ -> board
    Cells cells _ -> { board | value = Cells cells isActive }

activateBoard : BoardLocation -> Grid -> Grid
activateBoard boardLoc grid =
  let setActivity board =
        if board.location == boardLoc
        then setActive Active board
        else setActive Inactive board
  in
      { grid | data = A.map setActivity grid.data }

activateEntireGrid : Grid -> Grid
activateEntireGrid grid =
  { grid | data = A.map (setActive Active) grid.data }
