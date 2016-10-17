module GameDisplay exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import GameData exposing (..)
import GameInput exposing (..)
import Array as A

type alias Color = String
playerColor player =
  case player of
    User -> "blue"
    Computer -> "red"

wrapTableElement : Html.Html a -> Html.Html a
wrapTableElement elem = Html.td [] [elem]

makeTable : Int -> List (Html.Html a) -> Html.Html a
makeTable n elements =
  let
      rows = List.map (\row -> Html.tr [] row) (chunks n (List.map wrapTableElement elements))
  in
      Html.table [Attr.style [("padding", "10px")]] rows

makeBlob : Color -> List (Html.Html a) -> Html.Html a
makeBlob color elements =
  Html.div 
    [Attr.style [
      ("width", "109px"),
      ("height", "109px"),
      ("border-radius", "10px"),
      ("background-color", color)]]
    elements

viewCell : IsActive -> Cell -> Html.Html Input
viewCell isActive cell =
  let color = case cell.value of
                Empty -> "white"
                Filled player -> playerColor player
      style = Attr.style [("background-color", color), ("width", "25px"), ("height", "25px"), ("margin", "auto")]
  in case isActive of
      Active -> Html.button [ style, Events.onClick (Click cell.position) ] [ Html.text " " ]
      Inactive -> Html.div [ style ] [ Html.text " " ]

viewBoard : Board -> Html.Html Input
viewBoard board =
  case board.value of
    Cells cells isActive -> viewCells cells isActive
    WonBoard winner -> viewWonBoard winner

viewCells cells isActive =
  let
      element = makeTable 3 (List.map (viewCell isActive) (A.toList cells))
      color = case isActive of
        Active -> "yellow"
        Inactive -> "grey"
  in 
      makeBlob color [element]

viewWonBoard winner =
  makeBlob (playerColor winner) []

viewGrid : Grid -> Html.Html Input
viewGrid grid =
  let
      boardElements = A.map viewBoard grid.data
  in 
      makeTable 3 (A.toList boardElements)
