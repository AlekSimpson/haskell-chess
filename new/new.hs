module Main where

type Point = (Int, Int)
type BoardState = ([Int], [Int], [Int], [Int], [Int], [Int])

tget :: Point -> Int -> Int
tget (a, b) bit
  | bit == 0 = a
  | bit == 1 = b

 
conjunction :: Int -> Int -> Int
conjunction 0 0 = 0
conjunction 0 1 = 0
conjunction 1 0 = 0
conjunction 1 1 = 1
conjunction _ _ = -1

disjunction :: Int -> Int -> Int
disjunction 0 0 = 0
disjunction 0 1 = 1
disjunction 1 0 = 1
disjunction 1 1 = 1
disjunction _ _ = -1


and_:: [Int] -> [Int] -> [Int] 
and_ a b = map (\x -> conjunction (tget x 0) (tget x 1)) (zip a b)
or_ :: [Int] -> [Int] -> [Int]
or_ a b = map (\x -> disjunction (tget x 0) (tget x 1)) (zip a b)

makeBlank = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

pointToIndex :: Point -> Int
pointToIndex p = (((y - 1) `mod` 9) * 8) + (x `mod` 9) 
  where 
    y = tget p 1
    x = tget p 0


selectBoard :: [Int] -> Int -> Point -> [Int]
select square = selectBoard [] 64 square
selectBoard array count square
  | count == 0      = array 
  | count == select = selectBoard (1 : array) (count - 1) square
  | count /= 0      = selectBoard (0 : array) (count - 1) square
  where 
    select = pointToIndex square
-- select = (((y - 1) `mod` 9) * 8) + (x `mod` 9)
-- 2d point to 1d index formula: ((y % 9) * 8) + (x % 9)


rowBoard array count rowNum
  | count == 0                       = array
  | count <= upper && count >= lower = rowBoard (1 : array) (count - 1) rowNum
  | count < lower                    = rowBoard (0 : array) (count - 1) rowNum
  | count > upper                    = rowBoard (0 : array) (count - 1) rowNum
  where 
    upper = (rowNum `mod` 9) * 8
    lower = (((rowNum - 1) `mod` 9) * 8) + 1
-- row upper bound formula: (rowNum % 9) * 8
-- row lower bound formula: (((rowNum-1) % 9) * 8) + 1


multisel :: [Point] -> [Int] -> [Int]
multisel [] result = result
multisel points result = multisel (tail points) (or_ (select (points !! 0)) result)


-- the state of each side during the game will be held in a tuple containing six arrays, one for each piece. This function will help extract the needed set from the universal set
getPieceSet name (pawns, rooks, knights, bishops, queen, king)
  | name == "pawns" = pawns
  | name == "rooks" = rooks
  | name == "knights" = knights
  | name == "bishops" = bishops
  | name == "queen" = queen
  | name == "king" = king


-- piece-square-table
eval :: BoardState -> BoardState -> Int
eval blackPieces whitePieces = undefined

main = do 
  let points = [(4, 5), (2, 3), (5, 8)]
  let new = multisel points makeBlank

  putStrLn (show new)































