module Main where
import Text.Printf

-- DATA TYPE SECTION
data Color = Black | White deriving Show
data PieceName = King | Queen | Bishop | Knight | Rook | Pawn | Null deriving (Show)
data SquareState = Occupied | Empty

data Piece = Piece PieceName Color
data Square = Square Int Int Color Piece SquareState

instance Show Square where 
  -- show (Square x y _ _ _) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")  "
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

printSquare :: Square -> IO ()
printSquare sq = putStr (show sq)

printRowHelper :: [Square] -> Int -> IO ()
printRowHelper arr 0 = do 
  printSquare (arr !! 0)
  putStrLn ""
printRowHelper arr i = do 
  printSquare (arr !! i)
  printRowHelper arr (i-1)
printRow arr = printRowHelper arr 7

printBoard :: [[Square]] -> Int -> IO ()
printBoard board 0 = printRow (board !! 0)
printBoard board i = do 
  printRow (board !! i)
  printBoard board (i-1)

mutateRowHelper tp ap new = if (tp == ap) then new else ap
mutateRow target newSquare arr = map (\x -> mutateRowHelper target x newSquare) arr

makeSquare x y = Square x y (getColor (y + x)) nullPiece Empty

makeRow y = [makeSquare 1 y, makeSquare 2 y, makeSquare 3 y, makeSquare 4 y, makeSquare 5 y, makeSquare 6 y, makeSquare 7 y, makeSquare 8 y] 

getColor :: Int -> Color
getColor x = if (x `mod` 2) == 0 then White else Black

makeBoardHelper board 0 = board
makeBoardHelper board y = makeBoardHelper ((makeRow y) : board) (y - 1)
makeBoard = makeBoardHelper [] 8 
--END FUNCTIONS

main = do 
  let board = makeBoard

  printBoard board 7
  print "done"
