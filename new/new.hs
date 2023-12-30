module Main where

import Data.Bool

type Point = (Int, Int)
type PlayerState = ([Int], [Int], [Int], [Int], [Int], [Int])
type BoardState = (PlayerState, PlayerState) 
type Move = (Point, Point) -- start, end

showBoard :: BoardState -> Int -> IO ()
showBoard boardState targetPieces = undefined

bget :: BoardState -> Int -> PlayerState
bget (a, b) select
  | select == 0 = a
  | select == 1 = b


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

pawnBoardBias = [
   0,  0,   0,   0,   0,   0,  0,  0,
  50, 50,  50,  50,  50,  50, 50, 50,
  10, 10,  20,  30,  30,  20, 10, 10,
   5,  5,  10,  25,  25,  10,  5,  5,
   0,  0,   0,  20,  20,   0,  0,  0, 
   5, -5, -10,   0,   0, -10, -5,  5,
   5, 10,  10, -20, -20,  10, 10,  5,
   0,  0,   0,   0,   0,   0,  0,  0] :: [Int]
rookBoardBias = [
   0,  0,  0,  0,  0,  0,  0,  0,
   5, 10, 10, 10, 10, 10, 10,  5,
  -5,  0,  0,  0,  0,  0,  0, -5, 
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5, 
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5, 
   0,  0,  0,  0,  0,  0,  0,  0] :: [Int]
knightBoardBias = [
  -5, -4, -4, -4, -4, -4, -4, -5, 
  -4, -2,  0,  0,  0,  0, -2, -4, 
  -4,  0,  1,  2,  2,  1,  0, -4, 
  -4,  0,  2,  3,  3,  2,  0, -4, 
  -4,  0,  2,  3,  3,  2,  0, -4, 
  -4,  0,  1,  2,  2,  1,  0, -4, 
  -4, -2,  0,  0,  0,  0, -2, -4, 
  -5, -4, -4, -4, -4, -4, -4, -5] :: [Int]
bishopBoardBias = [
  -10,  -5,  -5,  -5,  -5,  -5,  -5, -10,
   -5,   0,   0,   0,   0,   0,   0,  -5,
   -5,   0,   5,  10,  10,   5,   0,  -5, 
   -5,   5,   5,  10,  10,   5,   5,  -5, 
   -5,   0,  10,  10,  10,  10,   0,  -5, 
   -5,  10,  10,  10,  10,  10,  10,  -5, 
   -5,   5,   0,   0,   0,   0,   5, -10,
  -10, -10, -10, -10, -10, -10, -10, -10] :: [Int]
queenBoardBias = [
  -20, -10, -10, -5, -5, -10, -10, -20, 
  -10,   0,   0,  0,  0,   0,   0, -10, 
  -10,   0,   5,  5,  5,   5,   0, -10,
   -5,   0,   5,  5,  5,   5,   0, -10, 
    0,   0,   5,  5,  5,   5,   0,  -5, 
  -10,   5,   5,  5,  5,   5,   0, -10, 
  -10,   0,   5,  0,  0,   0,   0, -10, 
  -20, -10, -10, -5, -5, -10, -10, -20] :: [Int]
kingBoardBias = [
  -30, -40, -40, -50, -50, -40, -40, -30,
  -30, -40, -40, -50, -50, -40, -40, -30,
  -30, -40, -40, -50, -50, -40, -40, -30,
  -30, -40, -40, -50, -50, -40, -40, -30,
  -20, -20, -20, -20, -20, -20, -20, -20,
   20,  20,   0,   0,   0,   0,  20,  20, 
   20,  30,  10,   0,   0,  10,  30,  20] :: [Int]

blackPawnBiases   = (reverse pawnBoardBias)
blackRookBiases   = (reverse rookBoardBias)
blackKnightBiases = (reverse knightBoardBias)
blackBishopBiases = (reverse bishopBoardBias)
blackQueenBiases  = (reverse queenBoardBias)
blackKingBiases   = (reverse kingBoardBias)

allBiases = (whiteBiases, blackBiases)
  where 
    whiteBiases = (pawnBoardBias, rookBoardBias, knightBoardBias, bishopBoardBias, queenBoardBias, kingBoardBias)
    blackBiases = (blackPawnBiases, blackRookBiases, blackKnightBiases, blackBishopBiases, blackQueenBiases, blackKingBiases)


pointToIndex :: Point -> Int
pointToIndex p = (((y - 1) `mod` 9) * 8) + (x `mod` 9) 
  where 
    y = tget p 1
    x = tget p 0


indexToPoint :: Int -> Point
indexToPoint = undefined


kingInCheck :: Int -> Bool
kingInCheck = undefined


pawnOnHomeRow :: Int -> Bool
pawnOnHomeRow = undefined


divFloat :: Int -> Int -> Fractional
divFloat a b = (a / b)
  where
    fa = fromIntegral a :: Float
    fb = fromIntegral b :: Float


-- input parameter is the index of the piece on the linearized board
isBishopMove :: Int -> Int -> Bool
isBishopMove start end = (div (y2 - y1) (x2 - x1)) == 1 || (div (y2 - y1) (x2 - x1)) == -1
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    y2 = tget endPoint 1
    y1 = tget startPoint 1
    x2 = tget endPoint 0
    x1 = tget startPoint 0

isRookMove :: Int -> Int -> Bool
isRookMove start end = (x2 == x1) || (y2 == y1)
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    x2 = tget endPoint 0
    x1 = tget startPoint 0
    y2 = tget endPoint 1
    y1 = tget endPoint 1

-- DOES NOT CHECK IF KING IS IN CHECK, NEED TO ADD THIS
isKingMove :: Int -> Int -> Bool
isKingMove start end = (x2 - x1) == 1 && (y2 - y1) == 1 && (not kingInCheck)
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    x1 = tget startPoint 0
    x2 = tget endPoint 0
    y1 = tget startPoint 1
    y2 = tget endPoint 1

-- NOTE: DOES NOT ACCOUNT FOR DIAGONAL CAPTURE YET
isPawnMove :: Int -> Int -> Bool -> Bool
isPawnMove start end onHomeRow
  | onHomeRow == True  = (y2 - y1) == 1 || (y2 - y1) == 2 
  | onHomeRow == False = (y2 - y1) == 1
  where
    startPoint = pointToIndex start
    endPoint = pointToIndex end
    y2 = tget endPoint 0
    y1 = tget startPoint 0

isKnightMove :: Int -> Int -> Bool
isKnightMove start end = ((slope == 2.0) || (slope == 0.5)) && ()
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    y2 = tget endPoint 1
    x2 = tget endPoint 0 
    y1 = tget startPoint 1
    x1 = tget startPoint 0
    slope = (abs (divFloat (y2 - y1) (x2 - x1)))


-- creates board with one piece placed on it, example return: [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0] (this array would have 64 elements)
select :: Point -> [Int]
select square = selectBoard [] 64 square

selectBoard :: [Int] -> Int -> Point -> [Int]
selectBoard array count square
  | count == 0      = array 
  | count == select = selectBoard (1 : array) (count - 1) square
  | count /= 0      = selectBoard (0 : array) (count - 1) square
  where 
    select = pointToIndex square
-- select = (((y - 1) `mod` 9) * 8) + (x `mod` 9)
-- 2d point to 1d index formula: ((y % 9) * 8) + (x % 9)

-- same as selectBoard except this is placing a row of pieces (mostly using this for pawns)
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

-- selects multiple arbitrary points on the board to place an arbitary type of piece
multisel :: [Point] -> [Int] -> [Int]
multisel [] result = result
multisel points result = multisel (tail points) (or_ (select (points !! 0)) result)


-- the state of each side during the game will be held in a tuple containing six arrays, one for each piece. This function will help extract the needed set from the universal set
getPieceSet :: Int -> PlayerState -> [Int]
getPieceSet id (pawns, rooks, knights, bishops, queen, king)
  | id == 1 = pawns
  | id == 2 = rooks
  | id == 3 = knights
  | id == 4 = bishops
  | id == 5 = queen
  | id == 6 = king


getPieceValue :: Int -> Int
getPieceValue id 
  | id == 1  = 10
  | id == 2  = 50
  | id == 3  = 30
  | id == 4  = 30
  | id == 5  = 90
  | id == 6  = 900

 
evalSide :: PlayerState -> PlayerState -> Int -> Int -> Int
evalSide pieceState biases 6 output  = output
evalSide pieceState biases id output = evalSide pieceState biases (id + 1) (output + (sum (map (\x -> (tget x 0) + (tget x 1)) occupiedSquares)))
  where
    pieceValue = getPieceValue id
    pieces = map (\x -> x * pieceValue) (getPieceSet id pieceState)
    pBiases = getPieceSet id biases 
    zipped = (zip pieces pBiases)
    occupiedSquares = filter (\x -> (tget x 0) == pieceValue) zipped


evalBoard :: BoardState -> BoardState -> Int
evalBoard boardState boardBiases = (evalSide (bget boardState 0) (bget boardBiases 0) 1 0) - (evalSide (bget boardState 1) (bget boardBiases 1) 1 0)


-- need a function that gets all legal moves for a piece: getLegalMoves
getPieceLegalMoves:: Point
getPieceLegalMoves = undefined

-- need a function that returns true or false depending on whether getLegalMoves returns an empty array for the king or not
gameIsOver :: BoardState -> Int
gameIsOver = undefined

-- minimax function, takes board and depth
-- get all legal moves
-- map eval to moves to get list of evals for each move 
minimax :: BoardState -> BoardState -> Move
minimax = undefined

main = do 
  -- CREATE STARTING POSITION
  let blackPawns   = multisel [(1, 2), (2, 2), (3, 2), (4, 2), (5, 2), (6, 2), (7, 2), (8, 2)] makeBlank
  let blackRooks   = multisel [(1, 1), (8, 1)] makeBlank
  let blackKnights = multisel [(2, 1), (7, 1)] makeBlank
  let blackBishops = multisel [(3, 1), (6, 1)] makeBlank
  let blackQueen   = multisel [(4, 1)] makeBlank
  let blackKing    = multisel [(5, 1)] makeBlank

  let whitePawns   = multisel [(1, 7), (2, 7), (3, 7), (4, 7), (5, 7), (6, 7), (7, 7), (8, 7)] makeBlank
  let whiteRooks   = multisel [(1, 8), (8, 8)] makeBlank
  let whiteKnights = multisel [(2, 8), (7, 8)] makeBlank
  let whiteBishops = multisel [(3, 8), (6, 8)] makeBlank
  let whiteQueen   = multisel [(4, 8)] makeBlank
  let whiteKing    = multisel [(5, 8)] makeBlank

  let whitePieces = (whitePawns, whiteRooks, whiteKnights, whiteBishops, whiteQueen, whiteKing) :: PlayerState
  let blackPieces = (blackPawns, blackRooks, blackKnights, blackBishops, blackQueen, blackKing) :: PlayerState

  let boardState = (whitePieces, blackPieces) :: BoardState
  
  let x = evalBoard boardState allBiases

  putStrLn (show x)
  putStrLn "done"































