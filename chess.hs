module Main where

import Data.Bool

type Point = (Int, Int)
type PlayerState = ([Int], [Int], [Int], [Int], [Int], [Int])
type BoardState = (PlayerState, PlayerState) 
type Move = (Point, Point) -- start, end
type MoveState = ([Move], [Move], [Move], [Move], [Move], [Move])


showBoard :: BoardState -> Int -> IO ()
showBoard boardState targetPieces = undefined


-- merges to arrays into one (NOTE: could just be replaced with `++`)
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


-- functions the same as `takeWhile` but when the predicate is not satisfied it includes the failed element and then stops
takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl _    []                = []
takeWhileIncl pred (x : xs) | pred x = x : takeWhileIncl pred xs
takeWhileIncl pred (x : xs)          = [x]


listToTuple6 :: [a] -> (a, a, a, a, a, a)
listToTuple6 [a, b, c, d, e, f] = (a, b, c, d, e, f)

tupleToList6 :: (a, a, a, a, a, a) -> [a]
tupleToList6 (a, b, c, d, e, f) = [a, b, c, d, e, f]

 
first (a, _) = a
second (_, b) = b

moveGet :: Move -> Int -> Point
moveGet (start, end) 0 = start
moveGet (start, end) 1 = end
moveGet (start, end) _ = start
 
mget :: MoveState -> Int -> [Move]
mget (a, b, c, d, e, f) select
  | select == 1 = a
  | select == 2 = b
  | select == 3 = c
  | select == 4 = d
  | select == 5 = e
  | select == 6 = f

bget :: BoardState -> Int -> PlayerState
bget (a, b) 0 = a
bget (a, b) 1 = b
bget _ _ = ([1], [1], [1], [1], [1], [1])

tget :: Point -> Int -> Int
tget (a, b) select
  | select == 0 = a
  | select == 1 = b


relativeIncrementX :: Int -> Int -> Int
relativeIncrementX 0 number = number - 1
relativeIncrementX 1 number = number + 1
relativeIncrementX _ number = number

 
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

xor :: Int -> Int -> Int
xor 0 0 = 0
xor 0 1 = 1
xor 1 0 = 1
xor 1 1 = 0
xor _ _ = -1

and_:: [Int] -> [Int] -> [Int] 
and_ a b = map (\x -> conjunction (tget x 0) (tget x 1)) (zip a b)

or_ :: [Int] -> [Int] -> [Int]
or_ a b = map (\x -> disjunction (tget x 0) (tget x 1)) (zip a b)

pickUpPiece :: [Int] -> [Int] -> [Int]
pickUpPiece a b = map (\x -> xor (tget x 0) (tget x 1)) (zip a b)


-- according to GHCi `1/0` equals infinity
inf :: Fractional a => a
inf = 1/0


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


invertColor :: Int -> Int
invertColor 0 = 1
invertColor 1 = 0
invertColor _ = -1


pointToIndex :: Point -> Int
pointToIndex p = (((y - 1) `mod` 9) * 8) + (x `mod` 9) 
  where 
    y = tget p 1
    x = tget p 0

indexToPoint :: Int -> Point
indexToPoint index = (x, y)
  where
    fi = fromIntegral index :: Float
    y = ceiling (fi / 8.0)
    x = (index - ((y - 1) * 8))


getPiecePositions :: BoardState -> Int -> Int -> [Point]
getPiecePositions board color piece = filteredPoints
  where
    sideState = bget board color
    pieceSet  = getPieceSet piece sideState
    filterIndices = filter (\(bit, index) -> bit == 1) (zip pieceSet [1..64])
    filteredPoints = map (\(bit, index) -> indexToPoint index) filterIndices


kingInCheck :: BoardState -> Int -> Int -> Bool
kingInCheck board color startIndex = (length collisions) > 0
  where
    selectIndex = select startIndex
    oppColor = invertColor color
    oppSideState = bget board oppColor
    oppSidePositions = [(getPiecePositions board oppColor x) | x <- [1..5]] :: [[Point]]
    zipped = (zip oppSidePositions [1..5]) :: [([Point], Int)]
    filterZipped = filter (\(points, piece) -> (length points) /= 0) zipped
    oppSideMoves = map (\(points, piece) -> collectPieceMoves board piece color points []) filterZipped
    collapsedMoves = collapse oppSideMoves
    uniqueMoves = removeDuplicates collapsedMoves 
    collisions = filter (\(start, end) -> startIndex == (pointToIndex end)) uniqueMoves


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)


collapse :: [[a]] -> [a]
collapse matrix = collapseHelp matrix []
collapseHelp [] accum = accum
collapseHelp matrix accum = collapseHelp (drop 1 matrix) ((head matrix) ++ accum)


collectPieceMoves :: BoardState -> Int -> Int -> [Point] -> [Move] -> [Move]
collectPieceMoves board piece color [] accum = accum
collectPieceMoves board piece color points accum = collectPieceMoves board piece color (drop 1 points) (moves ++ accum)
  where
    pointIndex = pointToIndex (head points)
    moves = getPieceMoves board piece color pointIndex


-- 0 => white, 1 => black
pawnOnHomeRow :: Int -> Int -> Bool
pawnOnHomeRow index 0 = (index >= 49 && index <= 56)
pawnOnHomeRow index 1 = (index >= 9  && index <= 16)
pawnOnHomeRow index _ = False


-- checks whether a square is occupied or not
checkSquareOccupied :: BoardState -> Point -> Bool 
checkSquareOccupied boardState point = sumCollisions > 0
  where
    index       = pointToIndex point
    selectIndex = select index
    whiteState  = bget boardState 0
    blackState  = bget boardState 1
    wRooks      = getPieceSet 2 whiteState
    wKnights    = getPieceSet 3 whiteState
    wBishops    = getPieceSet 4 whiteState
    wPawns      = getPieceSet 1 whiteState
    wKing       = getPieceSet 6 whiteState
    wQueen      = getPieceSet 5 whiteState
    bRooks      = getPieceSet 2 blackState
    bKnights    = getPieceSet 3 blackState
    bBishops    = getPieceSet 4 blackState
    bQueen      = getPieceSet 5 blackState
    bKing       = getPieceSet 6 blackState
    bPawns      = getPieceSet 1 blackState
    pieceStates = [wPawns, wRooks, wKnights, wBishops, wQueen, wKing, bPawns, bRooks, bKnights, bBishops, bQueen, bKing]
    collisionMaps = map (\pieceSet -> and_ pieceSet selectIndex) pieceStates
    sumCollisions = sum (map (\colMap -> sum colMap) collisionMaps)


checkSquareOccupiedWhite :: BoardState -> Point -> Bool
checkSquareOccupiedWhite boardState point = sumCollisions > 0
  where
    index = pointToIndex point
    selectIndex = select index
    whiteState = bget boardState 0
    wRooks      = getPieceSet 2 whiteState
    wKnights    = getPieceSet 3 whiteState
    wBishops    = getPieceSet 4 whiteState
    wPawns      = getPieceSet 1 whiteState
    wKing       = getPieceSet 6 whiteState
    wQueen      = getPieceSet 5 whiteState
    pieceStates = [wPawns, wRooks, wKnights, wBishops, wQueen, wKing]
    collisionMaps = map (\pieceState -> and_ pieceState selectIndex) pieceStates 
    sumCollisions = sum (map (\colMap -> sum colMap) collisionMaps)

checkSquareOccupiedBlack :: BoardState -> Point -> Bool
checkSquareOccupiedBlack boardState point = sumCollisions > 0
  where
    index = pointToIndex point
    selectIndex = select index
    blackState = bget boardState 1
    bRooks      = getPieceSet 2 blackState
    bKnights    = getPieceSet 3 blackState
    bBishops    = getPieceSet 4 blackState
    bQueen      = getPieceSet 5 blackState
    bKing       = getPieceSet 6 blackState
    bPawns      = getPieceSet 1 blackState
    pieceStates = [bPawns, bRooks, bKnights, bBishops, bQueen, bKing]
    collisionMaps = map (\pieceState -> and_ pieceState selectIndex) pieceStates 
    sumCollisions = sum (map (\colMap -> sum colMap) collisionMaps)

-- color: 0, 1 | piece: 1=>pawns, 2=>rooks, 3=>knights, 4=>bishops, 5=>queens, 6=>kings
checkSquareOccupiedPiece :: BoardState -> Int -> Int -> Point -> Bool
checkSquareOccupiedPiece boardState color piece point = (sum (and_ pieces selectIndex)) > 0
  where
    index = pointToIndex point
    selectIndex = select index
    state = bget boardState color
    pieces = getPieceSet piece state

-- input parameter is the index of the piece on the linearized board
isBishopMove :: Int -> Int -> Bool
isBishopMove start end = x2 /= x1 && (slope == 1 || slope == -1)
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    y2 = tget endPoint 1
    y1 = tget startPoint 1
    x2 = tget endPoint 0
    x1 = tget startPoint 0
    ydiff = fromIntegral (abs (y2 - y1)) :: Float
    xdiff = fromIntegral (abs (x2 - x1)) :: Float
    slope = ydiff / xdiff

getBishopMoves :: BoardState -> Int -> Int -> [Move]
getBishopMoves board color start = moveset
  where
    startPoint          = indexToPoint start
    startX              = tget startPoint 0
    startY              = tget startPoint 1
    allTheoreticalMoves = filter (\x -> isBishopMove start x) [1..64]
    theoryPoints        = map indexToPoint allTheoreticalMoves
    movesSE             = filter (\(x, y) -> (x < startX && y > startY)) theoryPoints
    movesSW             = filter (\(x, y) -> (x > startX && y > startY)) theoryPoints
    movesNE             = filter (\(x, y) -> (x < startX && y < startY)) theoryPoints
    movesNW             = filter (\(x, y) -> (x > startX && y < startY)) theoryPoints
    possibleNW          = takeWhileIncl (\point -> not (checkSquareOccupied board point)) (reverse movesNW)
    possibleNE          = takeWhileIncl (\point -> not (checkSquareOccupied board point)) (reverse movesNE)
    possibleSW          = takeWhileIncl (\point -> not (checkSquareOccupied board point)) movesSW
    possibleSE          = takeWhileIncl (\point -> not (checkSquareOccupied board point)) movesSE
    mergedMoves         = merge (merge possibleNW possibleNE) (merge possibleSW possibleSE)
    filteredOwnColor    = filter (\point -> not (filterOutOwnColor board color point)) mergedMoves
    moveset             = map (\point -> (startPoint, point)) filteredOwnColor

isRookMove :: Int -> Int -> Bool
isRookMove start end = (x2 == x1) || (y2 == y1)
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    x2 = tget endPoint 0
    x1 = tget startPoint 0
    y2 = tget endPoint 1
    y1 = tget startPoint 1

getRookMoves:: BoardState -> Int -> Int -> [Move]
getRookMoves board color start = moveset 
  where
    startPoint          = indexToPoint start
    startX              = tget startPoint 0
    startY              = tget startPoint 1
    allTheoreticalMoves = filter (\x -> isRookMove start x) [1..64]
    theoryPoints        = map indexToPoint allTheoreticalMoves
    northMoves          = filter (\(x, y) -> (x == startX && y < startY)) theoryPoints
    southMoves          = filter (\(x, y) -> (x == startX && y > startY)) theoryPoints
    eastMoves           = filter (\(x, y) -> (y == startY && x > startX)) theoryPoints
    westMoves           = filter (\(x, y) -> (y == startY && x < startX)) theoryPoints
    possibleNorthMoves  = takeWhileIncl (\point -> not (checkSquareOccupied board point)) (reverse northMoves)
    possibleSouthMoves  = takeWhileIncl (\point -> not (checkSquareOccupied board point)) southMoves
    possibleEastMoves   = takeWhileIncl (\point -> not (checkSquareOccupied board point)) eastMoves
    possibleWestMoves   = takeWhileIncl (\point -> not (checkSquareOccupied board point)) (reverse westMoves)
    mergedMoves         = merge (merge possibleSouthMoves possibleNorthMoves) (merge possibleWestMoves possibleEastMoves)
    filteredOwnColor    = filter (\point -> not (filterOutOwnColor board color point)) mergedMoves
    moveset             = map (\point -> (startPoint, point)) filteredOwnColor

isQueenMove :: Int -> Int -> Bool
isQueenMove start end = (isRookMove start end) || (isBishopMove start end)

getQueenMoves :: BoardState -> Int -> Int -> [Move]
getQueenMoves board color start = moveset
  where
    startPoint          = indexToPoint start
    startX              = tget startPoint 0
    startY              = tget startPoint 1
    allTheoreticalMoves = filter (\x -> isQueenMove start x) [1..64]
    theoryPoints        = map indexToPoint allTheoreticalMoves
    northMoves          = filter (\(x, y) -> (x == startX && y < startY)) theoryPoints
    southMoves          = filter (\(x, y) -> (x == startX && y > startY)) theoryPoints
    eastMoves           = filter (\(x, y) -> (y == startY && x > startX)) theoryPoints
    westMoves           = filter (\(x, y) -> (y == startY && x < startX)) theoryPoints 
    movesNW             = filter (\(x, y) -> (x < startX && y < startY)) theoryPoints
    movesNE             = filter (\(x, y) -> (x > startX && y < startY)) theoryPoints
    movesSW             = filter (\(x, y) -> (x < startX && y > startY)) theoryPoints
    movesSE             = filter (\(x, y) -> (x > startX && y > startY)) theoryPoints
    possibleNorth       = takeWhileIncl (\point -> not (checkSquareOccupied board point)) (reverse northMoves)
    possibleSouth       = takeWhileIncl (\point -> not (checkSquareOccupied board point)) southMoves
    possibleEast        = takeWhileIncl (\point -> not (checkSquareOccupied board point)) eastMoves
    possibleWest        = takeWhileIncl (\point -> not (checkSquareOccupied board point)) (reverse westMoves)
    possibleNW          = takeWhileIncl (\point -> not (checkSquareOccupied board point)) (reverse movesNW)
    possibleNE          = takeWhileIncl (\point -> not (checkSquareOccupied board point)) (reverse movesNE)
    possibleSW          = takeWhileIncl (\point -> not (checkSquareOccupied board point)) movesSW
    possibleSE          = takeWhileIncl (\point -> not (checkSquareOccupied board point)) movesSE
    mergeCardinal       = merge (merge possibleSouth possibleNorth) (merge possibleWest possibleEast)
    mergeDiagonal       = merge (merge possibleNE possibleNW) (merge possibleSW possibleSE)
    totalMerge          = filter (\point -> not (filterOutOwnColor board color point)) (merge mergeDiagonal mergeCardinal)
    moveset             = map (\point -> (startPoint, point)) totalMerge

isKingMove :: BoardState -> Int -> Int -> Int -> Bool
isKingMove board color start end = checkPos && (not (kingInCheck board color end)) && not (filterOutOwnColor board color (indexToPoint end)) 
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    x1 = tget startPoint 0
    x2 = tget endPoint 0
    y1 = tget startPoint 1
    y2 = tget endPoint 1
    xdiff = abs (x2 - x1)
    ydiff = abs (y2 - y1)
    checkPos = (xdiff == 1 && ydiff == 1) || (xdiff == 0 && ydiff == 1) || (xdiff == 1 && ydiff == 0)

getKingMoves :: BoardState -> Int -> Int -> [Move]
getKingMoves board color start = map (\point -> (startPoint, point)) notSameColorNotKing
  where
    startPoint = indexToPoint start
    allTheoreticalMoves = filter (\x -> isKingMove board color start x) [1..64]
    theoryPoints = map indexToPoint allTheoreticalMoves
    notSameColorNotKing = filter (\point -> not (filterOutOwnColor board color point) || (kingFilter board point)) theoryPoints

getInverseTest color 
  | color == 0 = -1
  | color == 1 = 1

isPawnMove :: BoardState -> Int -> Int -> Int -> Bool -> Bool
isPawnMove board color start end onHomeRow
  | onHomeRow == True  = (ydiff == correctDiff || ydiff == (correctDiff * 2)) && not (checkSquareOccupied board (indexToPoint end)) && (x2 == x1)
  | onHomeRow == False = ydiff == correctDiff && not (checkSquareOccupied board (indexToPoint end)) && (x2 == x1)
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    y2 = tget endPoint 1
    x2 = tget endPoint 0
    y1 = tget startPoint 1
    x1 = tget startPoint 0
    ydiff = (y2 - y1)
    correctDiff = getInverseTest color

getPawnMoves :: BoardState -> Int -> Int -> [Move]
getPawnMoves board color start = moveset
  where
    startPoint = indexToPoint start
    startX = tget startPoint 0
    startY = tget startPoint 1
    diagonals = [(startX + 1, relativeIncrementX color startY), (startX - 1, relativeIncrementX color startY)]
    diagonalPoints = filter (\point -> checkOppositeColor board color point) diagonals
    onHomeRow = pawnOnHomeRow start color
    theoryMoves = filter (\x -> isPawnMove board color start x onHomeRow) [1..64]
    possibleMoves = takeWhile (\x -> not (checkSquareOccupied board (indexToPoint x))) theoryMoves
    pmoves = map indexToPoint possibleMoves
    moveset = map (\x -> (startPoint, x)) (merge pmoves diagonalPoints)

isKnightMove :: BoardState -> Int -> Int -> Int -> Bool
isKnightMove board color start end = ((slope == 2.0) || (slope == 0.5)) && (xdiff <= 2 && ydiff <= 2) && not (filterOutOwnColor board color (indexToPoint end))
  where
    startPoint = indexToPoint start
    endPoint = indexToPoint end
    y2 = tget endPoint 1
    x2 = tget endPoint 0 
    y1 = tget startPoint 1
    x1 = tget startPoint 0
    ydiff = fromIntegral (abs (y2 - y1)) :: Float
    xdiff = fromIntegral (abs (x2 - x1)) :: Float
    slope = abs (ydiff / xdiff)

getKnightMoves :: BoardState -> Int -> Int -> [Move]
getKnightMoves board color start = moveset
  where
    startPoint  = indexToPoint start
    theoryMoves = filter (\x -> isKnightMove board color start x) [1..64] 
    theoryPoints = map indexToPoint theoryMoves
    moveset = map (\point -> (startPoint, point)) theoryPoints

-- need a function that gets all legal moves for a piece: getPieceMoves
getPieceMoves :: BoardState -> Int -> Int -> Int -> [Move]
getPieceMoves board piece color start
  | piece == 1 = getPawnMoves board color start 
  | piece == 2 = getRookMoves board color start
  | piece == 3 = getKnightMoves board color start
  | piece == 4 = getBishopMoves board color start
  | piece == 5 = getQueenMoves board color start
  | piece == 6 = getKingMoves board color start


getAllMovesForPieceType :: BoardState -> Int -> Int -> [Point] -> [Move]
getAllMovesForPieceType board piece color starts = collapseMoves
  where
    pointsToIndices = map (pointToIndex) starts
    moves = map (\start -> getPieceMoves board piece color start) pointsToIndices
    collapseMoves = foldl1 (++) moves


getAllLegalMoves :: BoardState -> Int -> [Move]
getAllLegalMoves board color = result
  where
    positions = map (\piece -> getPiecePositions board color piece) [1..6]
    rawResult = map (\(points, piece) -> getAllMovesForPieceType board piece color points) (zip positions [1..6])
    result = foldl1 (++) rawResult


filterOutOwnColor :: BoardState -> Int -> Point -> Bool
filterOutOwnColor board color point
  | color == 0 = checkSquareOccupiedWhite board point
  | color == 1 = checkSquareOccupiedBlack board point


checkOppositeColor :: BoardState -> Int -> Point -> Bool
checkOppositeColor board color point
  | color == 0 = checkSquareOccupiedBlack board point
  | color == 1 = checkSquareOccupiedWhite board point


kingFilter :: BoardState -> Point -> Bool
kingFilter board point = (sum (and_ whiteKing selectIndex)) > 0 || (sum (and_ blackKing selectIndex)) > 0
  where
    index = pointToIndex point
    selectIndex = select index
    whiteState = bget board 0
    blackState = bget board 1
    whiteKing = getPieceSet 6 whiteState
    blackKing = getPieceSet 6 blackState


-- creates board with one piece placed on it, example return: [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0] (this array would have 64 elements)
select :: Int -> [Int]
select square = selectBoard [] 64 (indexToPoint square)

selectp :: Point -> [Int]
selectp square = selectBoard [] 64 square

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
multisel :: [Point] -> [Int]
multisel points = multiselHelper points makeBlank
multiselHelper :: [Point] -> [Int] -> [Int]
multiselHelper [] result = result
multiselHelper points result = multiselHelper (tail points) (or_ (selectp (points !! 0)) result)


-- the state of each side during the game will be held in a tuple containing six arrays, one for each piece. This function will help extract the needed set from the universal set
getPieceSet :: Int -> PlayerState -> [Int]
getPieceSet 1 (pawns, rooks, knights, bishops, queen, king) = pawns 
getPieceSet 2 (pawns, rooks, knights, bishops, queen, king) = rooks
getPieceSet 3 (pawns, rooks, knights, bishops, queen, king) = knights
getPieceSet 4 (pawns, rooks, knights, bishops, queen, king) = bishops
getPieceSet 5 (pawns, rooks, knights, bishops, queen, king) = queen
getPieceSet 6 (pawns, rooks, knights, bishops, queen, king) = king
getPieceSet _ (pawns, rooks, knights, bishops, queen, king) = pawns


getPieceValue :: Int -> Int
getPieceValue select
  | select == 1  = 10
  | select == 2  = 50
  | select == 3  = 30
  | select == 4  = 30
  | select == 5  = 90
  | select == 6  = 900

 
-- eval function
evalSide :: PlayerState -> PlayerState -> Int -> Int -> Int
evalSide pieceState biases 6 output  = output
evalSide pieceState biases id output = evalSide pieceState biases (id + 1) (output + (sum (map (\x -> (tget x 0) + (tget x 1)) occupiedSquares)))
  where
    pieceValue = getPieceValue id
    pieces = map (\x -> x * pieceValue) (getPieceSet id pieceState)
    pBiases = getPieceSet id biases 
    zipped = (zip pieces pBiases)
    occupiedSquares = filter (\x -> (tget x 0) == pieceValue) zipped


-- evaluates an arbitrary on the board and outputs either a negative or positive value, positive indicates the position is good for white, negative indicates the position is good for black
evalBoard :: BoardState -> Float
evalBoard boardState = unscaledEvalF / 10.0
  where
    boardBiases = allBiases
    unscaledEval = ((evalSide (bget boardState 0) (bget boardBiases 0) 1 0) - (evalSide (bget boardState 1) (bget boardBiases 1) 1 0))
    unscaledEvalF = fromIntegral unscaledEval :: Float


-- need a function that returns true or false depending on whether getLegalMoves returns an empty array for the king or not
gameIsOver :: BoardState -> Bool
gameIsOver (white, black) = blackState || whiteState
  where
    whiteKing = getPieceSet 6 white
    whiteKPos = second (head (filter (\(bit, index) -> bit == 1) (zip whiteKing [1..64])))
    blackKing = getPieceSet 6 black
    blackKPos = second (head (filter (\(bit, index) -> bit == 1) (zip blackKing [1..64])))
    blackState = gameIsOverHelper (white, black) blackKPos 1
    whiteState = gameIsOverHelper (white, black) whiteKPos 0
    
gameIsOverHelper :: BoardState -> Int -> Int -> Bool
gameIsOverHelper board kingIndex color = (length (getKingMoves board color kingIndex)) == 0


transformSet :: [Int] -> [Int] -> Int -> Int -> [Int]
transformSet pieceSet newSet p searchPiece
  | p == searchPiece = newSet
  | p /= searchPiece = pieceSet

moveWhitePiece :: BoardState -> Int -> Bool -> Move -> BoardState
moveWhitePiece (white, black) piece isCapture move
  | isCapture == True  = (newPlayerState, newBlackState)
  | isCapture == False = (newPlayerState, black)
  where
    start = moveGet move 0
    end = moveGet move 1
    pieceSet = getPieceSet piece white
    occList = filter (\(set, i) -> checkSquareOccupiedBlack (white, black) end) (zip (tupleToList6 black) [1..6])
    occupied = head occList
    capturedPiece = second occupied
    blackPieceSet = first occupied
    pickUp = pickUpPiece pieceSet (multisel [start])
    remove = pickUpPiece blackPieceSet (multisel [end])
    place = or_ pickUp (multisel [end])
    newPiecesList = map (\(pieceSet, p) -> transformSet pieceSet place p piece) (zip (tupleToList6 white) [1..6])
    newBlackPiecesList = map (\(pieceSet, p) -> transformSet blackPieceSet remove p capturedPiece) (zip (tupleToList6 black) [1..6])
    newBlackState = listToTuple6 newBlackPiecesList
    newPlayerState = listToTuple6 newPiecesList

moveBlackPiece :: BoardState -> Int -> Bool -> Move -> BoardState
moveBlackPiece (white, black) piece isCapture move
  | isCapture == True  = (newWhiteState, newPlayerState)
  | isCapture == False = (white, newPlayerState)
  where
    start = moveGet move 0
    end = moveGet move 1
    pieceSet = getPieceSet piece black
    occList = filter (\(set, i) -> checkSquareOccupiedWhite (white, black) end) (zip (tupleToList6 white) [1..6])
    occupied = head occList
    capturedPiece = second occupied
    whitePieceSet = first occupied
    pickUp = pickUpPiece pieceSet (multisel [start])
    remove = pickUpPiece whitePieceSet (multisel [end])
    place = or_ pickUp (multisel [end])
    newPiecesList = map (\(pieceSet, p) -> transformSet pieceSet place p piece) (zip (tupleToList6 black) [1..6])
    newWhitePiecesList = map (\(pieceSet, p) -> transformSet whitePieceSet remove p capturedPiece) (zip (tupleToList6 white) [1..6])
    newWhiteState = listToTuple6 newWhitePiecesList
    newPlayerState = listToTuple6 newPiecesList

movePiece :: BoardState -> Int -> Bool -> Move -> Bool -> BoardState
movePiece board piece isCapture move isWhite
  | isWhite == True  = moveWhitePiece board piece isCapture move
  | isWhite == False = moveBlackPiece board piece isCapture move

makeMoves :: BoardState -> Int -> [Move] -> Bool -> [BoardState]
makeMoves board piece moves isWhite = map (\move -> movePiece board piece (checkSquareOccupied board (moveGet move 1)) move isWhite) moves
  where
    isCapture = checkSquareOccupied board 


minimaxHelper :: BoardState -> Int -> Int -> [Point] -> [Move] -> [Move]
minimaxHelper _ _ _ [] accum = accum
minimaxHelper board piece color points accum = minimaxHelper board piece color (tail points) (resultingMoves ++ accum)
  where
    currPoint = head points
    resultingMoves = (getPieceMoves board piece color (pointToIndex currPoint))

-- minimax function
minimax :: BoardState -> Int -> Int -> Float
minimax board depth color
  -- | (gameIsOver board) == True = evalBoard board
  | depth == 0 = evalBoard board
  | color == 0 = foldl1 (max) (positionToEvals) -- color is white
  | color == 1 = foldl1 (min) (positionToEvals) -- color is black
  where 
    oppSidePositions = [(getPiecePositions board (invertColor color) x) | x <- [1..6]] :: [[Point]]
    zipPosPiece = zip oppSidePositions [1..6]
    resultingMoves = map (\(list, piece) -> minimaxHelper board piece color list []) zipPosPiece :: [[Move]]
    resultingPositions = map (\(moveList, piece) -> makeMoves board piece moveList (color == 0)) (zip resultingMoves [1..6])
    collapseToOneD = foldl1 (++) resultingPositions :: [BoardState]
    positionToEvals = (map (\b -> minimax b (depth - 1) (invertColor color)) collapseToOneD)


defaultBest = (((0,0), (0,0)), 0.0)

engineGetPieceToMove :: BoardState -> Int -> Point -> Int
engineGetPieceToMove board color start = foundMatch -- pieceToMove
  where
    colorState = bget board color
    colorStateList = tupleToList6 colorState
    matchResults = map (\bitboard -> (sum (and_ bitboard (selectp start)))) colorStateList
    zipMatchResults = zip matchResults [1..6]
    foundMatchTuple = filter (\(bit, piece) -> bit == 1) zipMatchResults 
    foundMatch = second (head foundMatchTuple)


engineMax :: (Move, Float) -> (Move, Float) -> (Move, Float)
engineMax tA tB
  | (max evalA evalB) == evalB = tB
  | (max evalA evalB) == evalA = tA
  where
    evalA = second tA
    evalB = second tB


engine :: BoardState -> Int -> Int -> [Move] -> [Move]
engine board 0 color moves = reverse moves 
engine board maxMoves_ color moves = engine newBoard (maxMoves_ - 1) (invertColor color) (bestMove : moves)
  where
    legalMoves = getAllLegalMoves board color :: [Move]
    newBoards = map (\move -> (movePiece board (engineGetPieceToMove board color (moveGet move 0)) (checkSquareOccupied board (moveGet move 1)) move (color == 0))) legalMoves
    evalMoves = map (\b -> minimax b 2 (invertColor color)) newBoards
    zipEvalMoves = zip legalMoves evalMoves :: [(Move, Float)]
    bestMoveEval = foldl (\ta tb -> engineMax ta tb) (defaultBest) zipEvalMoves :: (Move, Float)
    bestMove = first bestMoveEval :: Move
    pieceToMove = engineGetPieceToMove board color (first bestMove)
    isCapture = checkSquareOccupied board (second bestMove)
    newBoard = movePiece board pieceToMove isCapture bestMove (color == 0) 


main = do 
  -- CREATE STARTING POSITION
  -- let blackPawns   = multisel [(x, 2) | x <- [1..8]]
  let blackPawns = multisel [(3, 5)]
  -- let blackRooks   = multisel [(1, 1), (8, 1)]
  -- let blackKnights = multisel [(2, 1), (7, 1)]
  -- let blackBishops = multisel [(3, 1), (6, 1)]
  -- let blackQueen   = multisel [(4, 1)]
  -- let blackKing    = multisel [(5, 1)]
  let blackKing = multisel [(7, 2)]

  -- let whitePawns   = multisel [(x, 7) | x <- [1..8]]
  -- let whiteRooks   = multisel [(1, 8), (8, 8)]
  let whiteRooks = multisel [(8, 1)]
  -- let whiteKnights = multisel [(2, 8), (7, 8)]
  -- let whiteBishops = multisel [(3, 8), (6, 8)]
  -- let whiteQueen   = multisel [(4, 8)]
  -- let whiteKing    = multisel [(5, 8)]
  let whiteKing = multisel [(3, 6)]

  let whitePieces = (makeBlank, whiteRooks, makeBlank, makeBlank, makeBlank, whiteKing) :: PlayerState
  let blackPieces = (blackPawns, makeBlank, makeBlank, makeBlank, makeBlank, blackKing) :: PlayerState

  let boardState = (whitePieces, blackPieces) :: BoardState

  let game = engine boardState 5 0 []
  let convert = map (\(startp, endp) -> (pointToIndex startp, pointToIndex endp)) game

  putStrLn (show convert)
  putStrLn "done"



-- stats:
--    ~20 seconds for 5 moves
--    ~32 seconds for 6 moves






























