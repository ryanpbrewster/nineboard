module GameDisplay exposing (..)

import Html
import Html.Attributes as Attr
import GameData exposing (..)

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

