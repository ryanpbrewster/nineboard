module GameData exposing (..)

import Array as A
import Maybe exposing (..)
import Debug

type Player = User | Computer

otherPlayer : Player -> Player
otherPlayer player =
  case player of
    User -> Computer
    Computer -> User

type alias BoardLocation = { i: Int, j: Int }
type alias CellLocation = { r: Int, c: Int }
type alias Position = { board: BoardLocation, cell: CellLocation }

type CellValue = Empty | Blue | Red
type alias Cell = { position: Position, value: CellValue }

type alias Board = {
  data : A.Array Cell,
  location: BoardLocation }

type alias Grid = {
  data : A.Array Board }

chunks : Int -> List a -> List (List a)
chunks n xs =
  if List.isEmpty xs 
  then [] 
  else List.take n xs :: chunks n (List.drop n xs)


emptyBoard : BoardLocation -> Board
emptyBoard location = {
  location = location,
  data = A.fromList
           [ { position = { board = location, cell = {r=0,c=0} }, value = Empty },
             { position = { board = location, cell = {r=0,c=1} }, value = Empty },
             { position = { board = location, cell = {r=0,c=2} }, value = Empty },
             { position = { board = location, cell = {r=1,c=0} }, value = Empty },
             { position = { board = location, cell = {r=1,c=1} }, value = Empty },
             { position = { board = location, cell = {r=1,c=2} }, value = Empty },
             { position = { board = location, cell = {r=2,c=0} }, value = Empty },
             { position = { board = location, cell = {r=2,c=1} }, value = Empty },
             { position = { board = location, cell = {r=2,c=2} }, value = Empty } ] }

boardRows : Board -> List (List Cell)
boardRows board = chunks 3 (A.toList board.data)

emptyGrid : Grid
emptyGrid = {
  data = A.fromList [ 
           emptyBoard {i=0,j=0}, emptyBoard {i=0,j=1}, emptyBoard {i=0,j=2},
           emptyBoard {i=1,j=0}, emptyBoard {i=1,j=1}, emptyBoard {i=1,j=2},
           emptyBoard {i=2,j=0}, emptyBoard {i=2,j=1}, emptyBoard {i=2,j=2} ] }

gridRows : Grid -> List (List Board)
gridRows grid = chunks 3 (A.toList grid.data)

extractBoard : BoardLocation -> Grid -> Maybe Board
extractBoard loc grid = A.get (3*loc.i + loc.j) grid.data

extractCell : CellLocation -> Board -> Maybe Cell
extractCell loc board = A.get (3*loc.r + loc.c) board.data

getCell : Position -> Grid -> Maybe Cell
getCell pos grid = Just grid `andThen` extractBoard pos.board `andThen` extractCell pos.cell

insertBoard : Board -> Grid -> Grid
insertBoard board grid =
  let idx = 3*board.location.i + board.location.j
  in { grid | data = A.set idx board grid.data }

insertCell : Cell -> Board -> Board
insertCell cell board =
  let idx = 3 * cell.position.cell.r + cell.position.cell.c
  in { board | data = A.set idx cell board.data }

setCell pos value grid =
  withDefault grid <|
    (Debug.log "board" <| extractBoard pos.board grid) `andThen` \board ->
      (Debug.log "cell" <| extractCell pos.cell board) `andThen` \cell ->
        Just <| insertBoard (insertCell { cell | value = value } board) grid
