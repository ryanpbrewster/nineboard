module GameData exposing (..)

import Array as A
import Set as S
import Maybe exposing (..)

type Player = User | Computer

otherPlayer : Player -> Player
otherPlayer player =
  case player of
    User -> Computer
    Computer -> User

type alias BoardLocation = { i: Int, j: Int }
type alias CellLocation = { r: Int, c: Int }
type alias Position = { board: BoardLocation, cell: CellLocation }

type CellValue = Empty | Filled Player
type alias Cell = 
  { position: Position
  , value: CellValue
  }

type IsActive = Active | Inactive
type BoardValue = Cells (A.Array Cell) IsActive
                | WonBoard Player
type alias Board = 
  { location: BoardLocation
  , value : BoardValue
  }

type alias Grid = 
  { data : A.Array Board 
  }

chunks : Int -> List a -> List (List a)
chunks n xs =
  if List.isEmpty xs 
  then [] 
  else List.take n xs :: chunks n (List.drop n xs)


emptyBoard : BoardLocation -> Board
emptyBoard boardLoc =
  let
      mkCell cellLoc = 
        { position = { board = boardLoc, cell = cellLoc }, value = Empty }
      cells = 
        [ mkCell {r=0,c=0}, mkCell {r=0,c=1}, mkCell {r=0,c=2} 
        , mkCell {r=1,c=0}, mkCell {r=1,c=1}, mkCell {r=1,c=2} 
        , mkCell {r=2,c=0}, mkCell {r=2,c=1}, mkCell {r=2,c=2}
        ]
  in 
      { location = boardLoc
      , value = Cells (A.fromList cells) Active
      }

emptyGrid : Grid
emptyGrid =
  { data = A.fromList [ 
           emptyBoard {i=0,j=0}, emptyBoard {i=0,j=1}, emptyBoard {i=0,j=2},
           emptyBoard {i=1,j=0}, emptyBoard {i=1,j=1}, emptyBoard {i=1,j=2},
           emptyBoard {i=2,j=0}, emptyBoard {i=2,j=1}, emptyBoard {i=2,j=2} ]
  }

gridRows : Grid -> List (List Board)
gridRows grid = chunks 3 (A.toList grid.data)

extractBoard : BoardLocation -> Grid -> Maybe Board
extractBoard loc grid = A.get (3*loc.i + loc.j) grid.data

extractCell : CellLocation -> Board -> Maybe Cell
extractCell loc board =
  case board.value of
    WonBoard _ -> Nothing
    Cells cells _ -> A.get (3*loc.r + loc.c) cells

getCell : Position -> Grid -> Maybe Cell
getCell pos grid = Just grid `andThen` extractBoard pos.board `andThen` extractCell pos.cell

insertBoard : Board -> Grid -> Grid
insertBoard board grid =
  let idx = 3*board.location.i + board.location.j
  in { grid | data = A.set idx board grid.data }

insertCell : Cell -> Board -> Board
insertCell cell board =
  case board.value of
    WonBoard _ -> board
    Cells cells isActive ->
      let 
          idx = 3 * cell.position.cell.r + cell.position.cell.c
          newCells = A.set idx cell cells
      in case boardWinner newCells of
          Just winner -> 
            { board | value = WonBoard winner }
          Nothing ->
            { board | value = Cells newCells isActive }

boardWinner : A.Array Cell -> Maybe Player
boardWinner cells =
  let 
      extractLine line = List.filterMap (\idx -> A.get idx cells) line
      lines = List.map extractLine possibleWinLines
  in
      oneOf (List.map lineWinner lines)

find : (a -> Bool) -> List a -> Maybe a
find pred xs =
  case xs of
    [] -> Nothing
    x :: xs' -> if pred x then Just x else find pred xs'

lineWinner : List Cell -> Maybe Player
lineWinner cells =
  let getPlayer cell = case cell.value of
                         Empty -> Nothing
                         Filled player -> Just player
      players = List.filterMap getPlayer cells
  in if (List.length players == 3 && allSame players)
     then List.head players
     else Nothing

allSame : List a -> Bool
allSame xs =
  case List.head xs of
    Nothing -> True
    Just x0 -> List.all (\x -> x == x0) xs

possibleWinLines = 
  [ [0,1,2], [3,4,5], [6,7,8],
    [0,3,6], [1,4,7], [2,5,8],
    [0,4,8], [2,4,6] ]
