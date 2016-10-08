module GameDisplay exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import GameData exposing (..)
import GameInput exposing (..)
import Array as A

viewCell : Cell -> Html.Html Input
viewCell cell =
  let color = case cell.value of
                Empty -> "white"
                Blue -> "blue"
                Red -> "red"
      action = Events.onClick (Click cell.position)
      style = Attr.style [("background-color", color), ("width", "25px"), ("height", "25px")]
      button = Html.button [ action, style ] [ Html.text " " ]
  in Html.td [] [button]

viewRow : List Cell -> Html.Html Input
viewRow row = Html.tr [] (List.map viewCell row)

viewBoard : Board -> Html.Html Input
viewBoard board =
  Html.table [Attr.style [("padding", "10px")]] (List.map viewRow (boardRows board))


viewBoardRow : List Board -> Html.Html Input
viewBoardRow row = Html.td [] (List.map viewBoard row)

viewGrid : Grid -> Html.Html Input
viewGrid grid =
  Html.table [] (List.map viewBoardRow (gridRows grid))
