module Main where
import Text.Printf

-- DATA TYPE SECTION
data Color = Black | White deriving Show
data PieceName = King | Queen | Bishop | Knight | Rook | Pawn | Null deriving (Show)
data SquareState = Occupied | Empty

data Piece = Piece PieceName Color
data Square = Square Int Int Color Piece SquareState

instance Show Square where 
  show (Square _ _ Black (Piece Null White) Empty)  = "\9632 " 
  show (Square _ _ White (Piece Null White) Empty)  = "\9633 " 
  show (Square _ _ _ (Piece King Black) Occupied)   = "\9818 " 
  show (Square _ _ _ (Piece Queen Black) Occupied)  = "\9819 "
  show (Square _ _ _ (Piece Bishop Black) Occupied) = "\9821 " 
  show (Square _ _ _ (Piece Knight Black) Occupied) = "\9822 " 
  show (Square _ _ _ (Piece Rook Black) Occupied)   = "\8920 " 
  show (Square _ _ _ (Piece Pawn Black) Occupied)   = "\9823 " 
  show (Square _ _ _ (Piece King White) Occupied)   = "\9818 " 
  show (Square _ _ _ (Piece Queen White) Occupied)  = "\9819 "
  show (Square _ _ _ (Piece Bishop White) Occupied) = "\9821 " 
  show (Square _ _ _ (Piece Knight White) Occupied) = "\9822 " 
  show (Square _ _ _ (Piece Rook White) Occupied)   = "\8920 " 
  show (Square _ _ _ (Piece Pawn White) Occupied)   = "\9823 "

instance Eq Square where 
  (==) (Square xOne yOne _ _ _) (Square xTwo yTwo _ _ _) = (xOne == xTwo) && (yOne == yTwo)
  (/=) (Square xOne yOne _ _ _) (Square xTwo yTwo _ _ _) = (xOne /= xTwo) || (yOne /= yTwo)
-- END DATA TYPE SECTION

-- FUNCTIONS
nullPiece = Piece Null White

printSquare sq = putStrLn (show sq)

printRow row 0 = do 
  printSquare (row !! 0)
  putStrLn ""
printRow row i = do
  printSquare (row !! i)
  printRow row (i-1)

-- print chess board
printBoardHelper 0 board = printRow (board !! 0) 7 
printBoardHelper i board = do 
  printRow (board !! i) 7 
  printBoardHelper (i-1) board
printBoard board = printBoardHelper 7 board

{-
Function: mutateRow
---------------
Updates a single square within a row

tp: target point, the point you want to update
ap: actual point, this is the point(s) that are iterated over in the map function. The map function sets this paramter
new: the new square to update the old square with

returns: a new row with the updated value
-}
mutateRowHelper tp ap new = if (tp == ap) then new else ap
mutateRow target newSquare arr = map (\x -> mutateRowHelper target x newSquare) arr

{-
Function: makeRow
---------------
Builds an 8 square long row

y: This is the y coordinate for the row

returns: 8x1 length array filled with empty squares
-}
makeRowHelper arr 0 y = arr
makeRowHelper arr x y = makeRowHelper ((Square x y (getColor (y + x)) nullPiece Empty) : arr) (x - 1) y 
makeRow y = makeRowHelper [] 8 y

{-
Function: getColor
---------------
This function will return either Black or White depending on whether the number passed in is even or odd

x: A positive integer value

returns: the color Black or White
-}
getColor :: Int -> Color
getColor x = if (x `mod` 2) == 0 then Black else White

{-
Function: makeBoard
---------------
Builds 8x8 chess board

returns: 8x8 matrix of type Square
-}
makeBoardHelper arr 8 = arr
makeBoardHelper arr y = makeBoardHelper ((makeRow y) : arr) (y + 1)
makeBoard = makeBoardHelper [] 1
--END FUNCTIONS

main = do 
  let board = makeBoard

  printBoard board
  print "done"
