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
makeRow x = [(1, x, Empty), (2, x, Empty), (3, x, Empty), (4, x, Empty), (5, x, Empty), (6, x, Empty), (7, x, Empty), (8, x, Empty)]


-- create chess board function
-- makeBoard :: Int -> Board -> Board
-- makeBoard 0 board = board
-- makeBoard count board = makeBoard (count-1) ((makeRow count) ++ board)

-- Print Row function
printRow :: Int -> [Square] -> IO ()
printRow 0 row = do putStrLn (show (row !! 0))
printRow i row = 
  do 
    putStr (show (row !! i))
    printRow (i-1) row

-- Print Matrix function
printMatrixHelper :: Board -> Int -> IO () 
printMatrixHelper matrix 0 = do putStrLn ""
printMatrixHelper matrix i = 
  do 
    printRow i (matrix !! i)
    printMatrixHelper matrix (i-1)

printMatrix :: Board -> IO ()
printMatrix matrix = printMatrixHelper matrix 7

-- main
main = do
  -- let board = makeBoard 9 []
  -- printMatrix board
  let board = [makeRow 9]
  let row = makeRow 9
  print (show (row ++ board))













