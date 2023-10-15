module Main where

data Piece = King | Queen | Bishop | Knight | Rook | Pawn deriving (Enum)
data SquareState = Empty | Piece deriving (Enum)

instance Show SquareState where 
  show Empty = "-"

instance Show Piece where
  show King = "K"
  show Queen = "Q"
  show Bishop = "B"
  show Knight = "Kn"
  show Rook = "R"
  show Pawn = "P"

type Square = (Int, Int, SquareState) 
type Board = [[Square]]


makeRow :: Int -> [(Int, Int, SquareState)]
makeRow x = [(0, x, Empty), (1, x, Empty), (2, x, Empty), (3, x, Empty), (4, x, Empty), (5, x, Empty), (6, x, Empty), (7, x, Empty)]

-- create chess board function
createChessBoard i row board | i == 0 = board -- base case
                             | not (i == 0) = createChessBoard (i-1) (makeRow (i-1)) (row : board)
initBoard :: Board
initBoard = createChessBoard 9 (makeRow 9) [] -- helper function

--querySquare square =  (square )
--query board row = map querySquare (board !! row)

printRow :: Int -> [Square] -> IO ()
printRow 0 row = 
  do 
    putStr (show (row !! 0))
    print ("\n")
printRow i row = 
  do 
    putStr (show (row !! i))
    printRow (i-1) row

printMatrix :: Board -> Int -> IO () 
printMatrix matrix 0 = do putStrLn ""
printMatrix matrix i = 
  do 
    printRow i (matrix !! i)
    printMatrix matrix (i-1)

printMatrix :: Board -> IO ()
printMatrix matrix = printMatrix matrix 9


main = do
  let board = initBoard
  printMatrix board
  print "success"
