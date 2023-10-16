module Main where

data Piece = King | Queen | Bishop | Knight | Rook | Pawn deriving (Enum)
data SquareState = Empty | Piece deriving (Enum)

-- TODO: Make the show for pieces and the board be unicode
instance Show SquareState where 
  show Empty = "#"

instance Show Piece where
  show King = "K"
  show Queen = "Q"
  show Bishop = "B"
  show Knight = "Kn"
  show Rook = "R"
  show Pawn = "P"

-- define board types
type Square = (Int, Int, SquareState) 
type Row = [Square]
type Board = [[Square]]

-- build row method
makeRow :: Int -> Row
makeRow x = [(1, x, Empty), (2, x, Empty), (3, x, Empty), (4, x, Empty), (5, x, Empty), (6, x, Empty), (7, x, Empty), (8, x, Empty)]

-- append a new row to the board
appendRow :: Row -> Board -> Board
appendRow row board = row : board

-- build entire chess board
buildBoardHelper :: Int -> Board -> Board
buildBoardHelper 0 board = board
buildBoardHelper y board = buildBoardHelper (y-1) (appendRow (makeRow y) board)

buildBoard :: Board
buildBoard = buildBoardHelper 8 []


-- Print Row function
printRow :: Int -> Row -> IO ()
printRow 0 row = do putStrLn (show (row !! 0))
printRow i row = 
  do 
    putStr (show (row !! i))
    printRow (i-1) row

-- Print Matrix function


-- main
main = do
  let board = buildBoard
  putStrLn (show board)












