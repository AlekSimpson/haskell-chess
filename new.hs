-- import Data.Array
module Main where

data Color = Black | White deriving Show
data PieceName = King | Queen | Bishop | Knigt | Rook | Pawn deriving (Show)

data Piece = Piece PieceName Color | 
data Square = Square Int Int Color Piece

instance Show Square where 
  show (Square _ _ _ _) = "A Square"
  show (Square _ _ Black Empty) = "B"
  show (Square _ _ White ) = "W"

instance Eq Square where 
  (==) (Square xOne yOne _ _) (Square xTwo yTwo _ _) = (xOne == xTwo) && (yOne == yTwo)
  (/=) (Square xOne yOne _ _) (Square xTwo yTwo _ _) = (xOne /= xTwo) || (yOne /= yTwo)

-- mapRow tP aP new = if (tP == aP) then 

main = do 
  -- let a = Point 1 3 10
  -- let b = Point 2 3 20
  -- print (a /= b)
  let arr = [Point 1 1 Black Nothing, Point 1 2 White Nothing, Point 1 3 Black Nothing, Point 1 4 White Nothing, Point 1 5 Black Nothing]
  print "done"
  -- let test = mapHelper 10 arr
  -- print (test)
  

  
