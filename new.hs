module Main where

-- DATA TYPE SECTION
data Color = Black | White deriving Show
data PieceName = King | Queen | Bishop | Knight | Rook | Pawn | Null deriving (Show)
data SquareState = Occupied | Empty

data Piece = Piece PieceName Color Int
pieceGetName (Piece name _ _) = name
pieceGetValue (Piece _ _ value) = value

data Square = Square Int Int Color Piece SquareState
squareGetY :: Square -> Int
squareGetY (Square _ y _ _ _) = y
squareGetX :: Square -> Int
squareGetX (Square x _ _ _ _) = x
squareGetP :: Square -> Piece
squareGetP (Square _ _ _ piece _) = piece
squareGetPN (Square _ _ _ piece _) = (pieceGetName piece)

data Point = Point Int Int
pointGetX (Point x _) = x
pointGetY (Point _ y) = y

instance Show Square where 
  -- show (Square x y _ _ _) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")  "
  show (Square _ _ Black (Piece Null White _) Empty)  = "\9632 " 
  show (Square _ _ White (Piece Null White _) Empty)  = "\9633 " 
  show (Square _ _ _ (Piece King White _) Occupied)   = "\9818 " 
  show (Square _ _ _ (Piece Queen White _) Occupied)  = "\9819 "
  show (Square _ _ _ (Piece Bishop White _) Occupied) = "\9821 " 
  show (Square _ _ _ (Piece Knight White _) Occupied) = "\9822 " 
  show (Square _ _ _ (Piece Rook White _) Occupied)   = "\9820 " 
  show (Square _ _ _ (Piece Pawn White _) Occupied)   = "\9823 " 

  show (Square _ _ _ (Piece King Black _) Occupied)   = "\9812 " 
  show (Square _ _ _ (Piece Queen Black _) Occupied)  = "\9813 "
  show (Square _ _ _ (Piece Bishop Black _) Occupied) = "\9815 " 
  show (Square _ _ _ (Piece Knight Black _) Occupied) = "\9816 " 
  show (Square _ _ _ (Piece Rook Black _) Occupied)   = "\9814 " 
  show (Square _ _ _ (Piece Pawn Black _) Occupied)   = "\9817 "

instance Eq Square where 
  (==) (Square xOne yOne _ _ _) (Square xTwo yTwo _ _ _) = (xOne == xTwo) && (yOne == yTwo)
  (/=) (Square xOne yOne _ _ _) (Square xTwo yTwo _ _ _) = (xOne /= xTwo) || (yOne /= yTwo)
-- END DATA TYPE SECTION

-- FUNCTIONS
nullPiece = Piece Null White 0

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

printBoardHelper :: [[Square]] -> Int -> IO ()
printBoardHelper board 0 = printRow (board !! 0)
printBoardHelper board i = do 
  printRow (board !! i)
  printBoardHelper board (i-1)
printBoard board = printBoardHelper board 7

makeSquare x y = Square x y (getColor (y + x)) nullPiece Empty
makeOccupiedSquare x y piece = Square x y (getColor (y + x)) piece Occupied

makeRow y = [makeSquare x y | x <- [1..8]]
makePawnRow y color = [makeOccupiedSquare x y (Piece Pawn color 1) | x <- [1..8]]

getColor :: Int -> Color
getColor x = if (x `mod` 2) == 0 then Black else White

makeBoardHelper board 0 = board
makeBoardHelper board y = makeBoardHelper ((makeRow y) : board) (y - 1)
makeBoard = makeBoardHelper [] 8 

mutateRowHelper :: Square -> Square -> Square
mutateRowHelper ap new = if (new == ap) then new else ap
mutateRow :: Square -> [Square] -> [Square]
mutateRow newSquare row = map (\x -> mutateRowHelper x newSquare) row

mutateBoard :: Square -> [[Square]] -> [[Square]]
mutateBoardHelper currRow newSquare = if ((squareGetY (currRow !! 0)) == (squareGetY newSquare)) then (mutateRow newSquare currRow) else currRow
mutateBoard newSquare board = map (\currRow -> mutateBoardHelper currRow newSquare) board

place :: [Square] -> [[Square]] -> [[Square]]
place [] board = board
place pieces board = place (drop 1 pieces) (mutateBoard (pieces !! 0) board)

makeTargets :: [(Point, Piece)] -> [Square]
makeTargets zippedPoints = map (\(point, piece) -> makeOccupiedSquare (pointGetX point) (pointGetY point) piece) zippedPoints

placeFromTargets [] board = board
placeFromTargets targetRows board = placeFromTargets (drop 1 targetRows) (place (targetRows !! 0) board)

eval :: [[Square]] -> [[Square]]
eval board = board

main = do 
  -- INITIALIZE BOARD
  let whitePawnRow  = [(Piece Pawn White 1), (Piece Pawn White 1), (Piece Pawn White 1), (Piece Pawn White 1), (Piece Pawn White 1), (Piece Pawn White 1), (Piece Pawn White 1), (Piece Pawn White 1)]
  let blackPawnRow  = [(Piece Pawn Black 1), (Piece Pawn Black 1), (Piece Pawn Black 1), (Piece Pawn Black 1), (Piece Pawn Black 1), (Piece Pawn Black 1), (Piece Pawn Black 1), (Piece Pawn Black 1)] 
  let blackInnerRow = [(Piece Rook Black 5), (Piece Knight Black 3), (Piece Bishop Black 4), (Piece Queen Black 9), (Piece King Black 0), (Piece Bishop Black 4), (Piece Knight Black 3), (Piece Rook Black 5)]
  let whiteInnerRow = [(Piece Rook White 5), (Piece Knight White 3), (Piece Bishop White 4), (Piece Queen White 9), (Piece King White 0), (Piece Bishop White 4), (Piece Knight White 3), (Piece Rook White 5)]
  let whitePawnPoints = [Point x 2 | x <- [1..8]]
  let blackPawnPoints = [Point x 7 | x <- [1..8]]
  let blackInnerRowPoints = [Point x 8 | x <- [1..8]]  
  let whiteInnerRowPoints = [Point x 1 | x <- [1..8]]

  let wPawnTargets = makeTargets (zip whitePawnPoints blackPawnRow)
  let bPawnTargets = makeTargets (zip blackPawnPoints blackPawnRow)
  let wInnerTargets = makeTargets (zip whiteInnerRowPoints whiteInnerRow)
  let bInnerTargets = makeTargets (zip blackInnerRowPoints blackInnerRow)

  let targets = [wPawnTargets, bPawnTargets, wInnerTargets, bInnerTargets]

  let board = placeFromTargets targets makeBoard

  -- START GAME LOOP
  -- function that takes current board state and then outputs the next best board state

  printBoard board
  print "done"












