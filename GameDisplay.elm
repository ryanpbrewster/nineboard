module GameDisplay exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import GameData exposing (..)
import GameInput exposing (..)
import Array as A

playerColor player =
  case player of
    User -> "blue"
    Computer -> "red"

wrapTableElement : Html.Html a -> Html.Html a
wrapTableElement elem = Html.td [] [elem]

viewCell : Cell -> Html.Html Input
viewCell cell =
  let color = case cell.value of
                Empty -> "white"
                Filled player -> playerColor player
      action = Events.onClick (Click cell.position)
      style = Attr.style [("background-color", color), ("width", "25px"), ("height", "25px"), ("margin", "auto")]
  in Html.button [ action, style ] [ Html.text " " ]

viewRow : List Cell -> Html.Html Input
viewRow row = Html.tr [] (List.map (viewCell >> wrapTableElement) row)

viewBoard : Board -> Html.Html Input
viewBoard board =
  case boardWinner board of
    Nothing ->
      Html.table
        [Attr.style [("padding", "10px")]]
        (List.map viewRow (boardRows board))
    Just player ->
      Html.div 
        [Attr.style [
          ("margin", "auto"),
          ("width", "89px"),
          ("height", "89px"),
          ("border-radius", "10px"),
          ("background-color", playerColor player)]]
        []


viewBoardRow : List Board -> Html.Html Input
viewBoardRow row = Html.tr [] (List.map (viewBoard >> wrapTableElement) row)

viewGrid : Grid -> Html.Html Input
viewGrid grid =
  Html.table [] (List.map viewBoardRow (gridRows grid))
