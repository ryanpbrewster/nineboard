import Html
import Html.Attributes as Attr
import Html.App as App
import Html.Events

main =
  App.beginnerProgram { model = emptyGrid, view = view, update = update }

type Cell = Empty | Blue | Red
type alias Board = { cells: List (List Cell) }
type alias Grid = { boards: List (List Board) }

emptyBoard : Board
emptyBoard = { 
  cells =  [ [Blue, Red, Empty]
           , [Red, Blue, Empty]
           , [Empty, Empty, Blue] ]
  }

emptyGrid : Grid
emptyGrid = {
  boards = [ [ emptyBoard, emptyBoard, emptyBoard ]
           , [ emptyBoard, emptyBoard, emptyBoard ]
           , [ emptyBoard, emptyBoard, emptyBoard ] ]
  }

type alias Model = Grid
type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model

    Decrement ->
      model

viewCell : Cell -> Html.Html a
viewCell cell =
  let color = case cell of
    Empty -> "white"
    Blue -> "blue"
    Red -> "red"
  in Html.td [Attr.style [("background-color", color), ("border", "1px solid black")]] [Html.text " "]

viewRow : List Cell -> Html.Html a
viewRow row = Html.tr [] (List.map viewCell row)

viewBoard : Board -> Html.Html a
viewBoard board =
  Html.table [Attr.style [("border", "1px solid black")]] (List.map viewRow board.cells)


viewBoardRow : List Board -> Html.Html a
viewBoardRow row = Html.td [] (List.map viewBoard row)

viewGrid : Grid -> Html.Html a
viewGrid grid =
  Html.table [] (List.map viewBoardRow grid.boards)

view = viewGrid
