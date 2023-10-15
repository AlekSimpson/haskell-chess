module Main where

data Piece = King | Queen | Bishop | Knight | Rook | Pawn deriving (Enum)
data SquareState = Empty | Piece

instance Show Piece where
  show King = "K"
  show Queen = "Q"
  show Bishop = "B"
  show Knight = "Kn"
  show Rook = "R"
  show Pawn = "P"

type Square = (Int, Int, SquareState)
type Board = [[Square]]

-- create chess board function
createChessBoard i row board | i == 0 = board -- base case
                             | not (i == 0) = createChessBoard (i-1) (row) (row : board)
initBoard :: Board
initBoard = createChessBoard 9 [(0, ), (), (), (), (), (), (), ()] [] -- helper function

--printMatrix :: Board -> Int -> IO ()
--printMatrix matrix 0 = do print matrix
--printMatrix matrix i = 
--  do 
--    print (matrix !! i)
--    printMatrix matrix (i-1)


main = do
  let board = initBoard
  print "success"
