module Display exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Data exposing (..)
import Input exposing (..)
import Util exposing (..)
import Array as A

view : GameState -> Html.Html Input
view state = displayGrid (configureOptions state) state.grid

type alias Options =
  { activeBoardColor : Color
  , inactiveBoardColor : Color
  , activeCellColor : Color
  , emptyCellColor : Color
  , mousePosition : Maybe Position
  }

configureOptions : GameState -> Options
configureOptions state =
  { activeBoardColor = "gold"
  , inactiveBoardColor = "silver"
  , emptyCellColor = "white"
  , activeCellColor = playerColor state.currentPlayer
  , mousePosition = state.mousePosition
  }

type alias Color = String
playerColor player =
  case player of
    User -> "green"
    Computer -> "red"

wrapTableElement : Html.Html a -> Html.Html a
wrapTableElement elem = Html.td [] [elem]

makeTable : Int -> List (Html.Html a) -> Html.Html a
makeTable n elements =
  let
      rows = List.map (\row -> Html.tr [] row) (chunks n (List.map wrapTableElement elements))
  in
      Html.table
        [Attr.style
          [ ("transform", "translate(-50%, -50%)")
          , ("top", "50%")
          , ("left", "50%")
          , ("position", "relative")
          ]
        ]
        rows

makeBlob : Color -> List (Html.Html a) -> Html.Html a
makeBlob color elements =
  Html.div
    [Attr.style
      [ ("width", "150px")
      , ("height", "150px")
      , ("border-radius", "15px")
      , ("background-color", color)
      , ("margin", "auto")
      ]
    ]
    elements

exists : Maybe a -> (a -> Bool) -> Bool
exists mb pred =
  case mb of
    Nothing -> False
    Just v -> pred v

flatten : List (Maybe a) -> List a
flatten xs =
  case xs of
    Just x :: xs_ -> x :: flatten xs_
    Nothing :: xs_ -> flatten xs_
    [] -> []

viewCell : Options -> IsActive -> Cell -> Html.Html Input
viewCell options isActive cell =
  let
      color = case cell.value of
          Filled player -> playerColor player
          Empty -> 
            if exists options.mousePosition (\pos -> pos == cell.position)
            then options.activeCellColor
            else options.emptyCellColor
      style = Attr.style
          [ ("background-color", color)
          , ("width", "40px")
          , ("height", "40px")
          , ("border-radius", "5px")
          , ("margin", "auto")
          ]
      events = flatten
        [ Just <| Events.onMouseOver (MouseOver cell.position)
        , case cell.value of
            Empty -> Just <| Events.onClick (Click cell.position)
            Filled _ -> Nothing
        ]
  in case (isActive, cell.value) of
        (Active, Empty) ->
            Html.button (style :: events) []

        _ ->
            Html.button [ style, Attr.disabled True ] []



viewBoard : Options -> Board -> Html.Html Input
viewBoard options board =
  case board.value of
    Cells cells isActive -> viewCells options cells isActive
    WonBoard winner -> viewWonBoard options winner

viewCells options cells isActive =
  let
      element = makeTable 3 (List.map (viewCell options isActive) (A.toList cells))
      color = case isActive of
        Active -> options.activeBoardColor
        Inactive -> options.inactiveBoardColor
  in
      makeBlob color [element]

viewWonBoard options winner =
  makeBlob (playerColor winner) []

displayGrid : Options -> Grid -> Html.Html Input
displayGrid options grid =
  let
      boardElements = A.map (viewBoard options) grid.data
  in
      Html.div
        [Attr.style
          [ ("width", "500px")
          , ("height", "500px")
          , ("margin", "auto")
          , ("padding", "25px")
          ]
        ]
        [makeTable 3 (A.toList boardElements)]
