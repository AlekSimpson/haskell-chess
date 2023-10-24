import Data.Array


data Piece = King | Nothing deriving (Show)
data Square = Square Int Int Piece deriving (Show)

-- instance Show Square where 
  -- show Square = "test"

getEl arr x y = do 
  let row = arr!x
  row!y

main :: IO ()
main = do 
  let a = listArray (0, 7) [(Square 1 1), (Square 1 2), (Square 1 3), (Square 1 4), (Square 1 5), (Square 1 6), (Square 1 7), (Square 1 8)]
  let b = listArray (0, 7) [a, a, a, a, a, a, a, a]
  let x = getEl b 1 3
  print x
  putStrLn "done"



