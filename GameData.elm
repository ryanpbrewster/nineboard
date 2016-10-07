module GameData exposing (..)

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
