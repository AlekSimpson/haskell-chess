module Main where
import Data.Array

data Piece = BKing | BQueen | BBishop | BKnight | BRook | BPawn |
             WKing | WQueen | WBishop | WKnight | WRook | WPawn deriving (Enum)
data SquareState = WEmpty | BEmpty | Piece deriving (Enum)

instance Show SquareState where 
  show WEmpty = "\9633 "
  show BEmpty = "\9632 "

instance Show Piece where
  show BKing   = "\9818"
  show BQueen  = "\9819"
  show BBishop = "\9821"
  show BKnight = "\9822"
  show BRook   = "\8920"
  show BPawn   = "\9823"
  show WKing   = "\9812"
  show WQueen  = "\9813"
  show WBishop = "\9815"
  show WKnight = "\9816"
  show WRook   = "\9814"
  show WPawn   = "\9817"

-- define board types
getLast :: (a, b, c) -> c
getLast (_, _, x) = x
type Square = (Int, Int, SquareState) 

-- type Row = [Square]
type Row = Array Square Square
type Board = [[Square]]

-- create new square tuple
makeSquare x y = (x, y, (getColor (y + x)))

-- decides what color square should be based on the sum of the coordinates
getColor :: Int -> SquareState
getColor x = if (x `mod` 2) == 0 then BEmpty else WEmpty

-- build row method
makeRow :: Int -> Row 
makeRow x = listArray (0, 7) [makeSquare 1 x, makeSquare 2 x, makeSquare 3 x, makeSquare 4 x, makeSquare 5 x, makeSquare 6 x, makeSquare 7 x, makeSquare 8 x]

-- prototypeBoard :: Int -> Array -> Array
-- prototypeBoard 0 board = board 
prototypeBoard = listArray (0, 7) [(i, (makeRow i)) | i <- [0..7]]

-- append a new row to the board
--appendRow :: Row -> Board -> Board
--appendRow row board = row : board
--
---- build entire chess board
--buildBoardHelper :: Int -> Board -> Board
--buildBoardHelper 0 board = board
--buildBoardHelper y board = buildBoardHelper (y-1) (appendRow (makeRow y) board)
--
--buildBoard :: Board
--buildBoard = buildBoardHelper 8 []
--
---- print single square
--printSquare :: Square -> IO ()
--printSquare square = putStr (show (getLast square))
--
---- print single row of squares
--printRow :: Row -> Int -> IO ()
--printRow row 0 = do 
--  printSquare (row !! 0)
--  putStrLn ""
--printRow row i = do
--  printSquare (row !! i)
--  printRow row (i-1)
--
---- print chess board
--printMatrix :: Int -> Board -> IO ()
--printMatrix 0 board = printRow (board !! 0) 7 
--printMatrix i board = do 
--  printRow (board !! i) 7 
--  printMatrix (i-1) board

-- main
main :: IO ()
main = do
  -- let board = buildBoard
  -- printMatrix 7 board
  let board = prototypeBoard
  let x = board!3
  print x



