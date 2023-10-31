module Main where
import Text.Printf

data Color = Black | White deriving Show
data PieceName = King | Queen | Bishop | Knigt | Rook | Pawn deriving (Show)
data SquareState = Occupied | Empty

data Piece = Piece PieceName Color
data Square = Square Int Int Color Piece SquareState

instance Show Square where 
  show (Square x y _ _ _) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Square where 
  (==) (Square xOne yOne _ _ _) (Square xTwo yTwo _ _ _) = (xOne == xTwo) && (yOne == yTwo)
  (/=) (Square xOne yOne _ _ _) (Square xTwo yTwo _ _ _) = (xOne /= xTwo) || (yOne /= yTwo)

mutateArray tp ap new = if (tp == ap) then new else ap

main = do 
  -- let a = Point 1 3 10
  -- let b = Point 2 3 20
  -- print (a /= b)
  let dummy = Piece King Black
  let arr = [Square 1 1 Black dummy Empty, Square 1 2 White dummy Empty, Square 1 3 Black dummy Empty, Square 1 4 White dummy Empty, Square 1 5 Black dummy Empty]
  let target = Square 1 2 White dummy Empty
  let newSquare = Square 5 7 White dummy Empty
  let test = map (\x -> mutateArray target x newSquare) arr
  print (show test)
  print "done"
  

  
