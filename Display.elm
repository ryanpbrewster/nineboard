module Display exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Data exposing (..)
import Input exposing (..)
import Array as A

view : GameState -> Html.Html Input
view state = displayGrid (configureOptions state) state.grid

type alias Options =
  { activeBoardColor: Color
  }

configureOptions : GameState -> Options
configureOptions state =
  { activeBoardColor = playerColor state.currentPlayer
  }

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

viewBoard : Options -> Board -> Html.Html Input
viewBoard options board =
  case board.value of
    Cells cells isActive -> viewCells options cells isActive
    WonBoard winner -> viewWonBoard options winner

viewCells options cells isActive =
  let
      element = makeTable 3 (List.map (viewCell isActive) (A.toList cells))
      color = case isActive of
        Active -> options.activeBoardColor
        Inactive -> "grey"
  in 
      makeBlob color [element]

viewWonBoard options winner =
  makeBlob (playerColor winner) []

displayGrid : Options -> Grid -> Html.Html Input
displayGrid options grid =
  let
      boardElements = A.map (viewBoard options) grid.data
  in 
      makeTable 3 (A.toList boardElements)