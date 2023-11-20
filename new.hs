module Main where

-- DATA TYPE SECTION
data Color = Black | White deriving Show
data PieceName = King | Queen | Bishop | Knight | Rook | Pawn | Null deriving (Show)
data SquareState = Occupied | Empty

data Piece = Piece PieceName Color
data Square = Square Int Int Color Piece SquareState

squareGetY :: Square -> Int
squareGetY (Square _ y _ _ _) = y
squareGetX :: Square -> Int
squareGetX (Square x _ _ _ _) = x
squareGetP :: Square -> Piece
squareGetP (Square _ _ _ piece _) = piece

instance Show Square where 
  -- show (Square x y _ _ _) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")  "
  show (Square _ _ Black (Piece Null White) Empty)  = "\9632 " 
  show (Square _ _ White (Piece Null White) Empty)  = "\9633 " 
  show (Square _ _ _ (Piece King White) Occupied)   = "\9818 " 
  show (Square _ _ _ (Piece Queen White) Occupied)  = "\9819 "
  show (Square _ _ _ (Piece Bishop White) Occupied) = "\9821 " 
  show (Square _ _ _ (Piece Knight White) Occupied) = "\9822 " 
  show (Square _ _ _ (Piece Rook White) Occupied)   = "\9820 " 
  show (Square _ _ _ (Piece Pawn White) Occupied)   = "\9823 " 

  show (Square _ _ _ (Piece King Black) Occupied)   = "\9812 " 
  show (Square _ _ _ (Piece Queen Black) Occupied)  = "\9813 "
  show (Square _ _ _ (Piece Bishop Black) Occupied) = "\9815 " 
  show (Square _ _ _ (Piece Knight Black) Occupied) = "\9816 " 
  show (Square _ _ _ (Piece Rook Black) Occupied)   = "\9814 " 
  show (Square _ _ _ (Piece Pawn Black) Occupied)   = "\9817 "

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

printBoardHelper :: [[Square]] -> Int -> IO ()
printBoardHelper board 0 = printRow (board !! 0)
printBoardHelper board i = do 
  printRow (board !! i)
  printBoardHelper board (i-1)
printBoard board = printBoardHelper board 7

makeSquare x y = Square x y (getColor (y + x)) nullPiece Empty
makeSquarePiece x y piece = Square x y (getColor (y + x)) piece Occupied

makeRow y = [makeSquare x y | x <- [1..8]]
makePawnRow y color = [makeSquarePiece x y (Piece Pawn color) | x <- [1..8]]

getColor :: Int -> Color
getColor x = if (x `mod` 2) == 0 then Black else White

makeBoardHelper board 0 = board
makeBoardHelper board y = makeBoardHelper ((makeRow y) : board) (y - 1)
makeBoard = makeBoardHelper [] 8 

makePieces color = [(Piece Pawn color), (Piece Rook color), (Piece Knight color), (Piece Bishop color), (Piece Queen color), (Piece King White)] 
makeStartRow :: Color -> [Piece]
makeStartRow c = [(Piece Rook c), (Piece Knight c), (Piece Bishop c), (Piece Queen c), (Piece King c), (Piece Bishop c), (Piece Knight c), (Piece Rook c)]

-- MUTATION FUNCTIONS
mutateRowHelper :: Square -> Square -> Square -> Square
mutateRowHelper tp ap new = if (tp == ap) then new else ap
mutateRow :: Square -> Square -> [Square] -> [Square]
mutateRow target newSquare row = map (\x -> mutateRowHelper target x newSquare) row

mutateBoard :: Square -> Square -> [[Square]] -> [[Square]]
mutateBoardHelper target row newSquare = if ((squareGetY target) == (squareGetY (row !! 0))) then (mutateRow target newSquare row) else row
mutateBoard targetSquare newSquare board = map (\currSquare -> mutateBoardHelper targetSquare currSquare newSquare) board
--END FUNCTIONS

-- take a board and initialize it with a starting position
placePiecesRow :: [[Square]] -> [Square] -> Int -> Int -> [Square] -> [[Square]]
placePiecesRow board targets 8 y pieces = board
placePiecesRow board targets x y pieces = placePiecesRow (mutateBoard (targets !! 0) (makeSquarePiece x y (squareGetP (pieces !! 0))) board) (drop 1 targets) (x + 1) y (drop 1 pieces)

placePieces board y pieces = placePieces (placePiecesRow board () 0 y pieces) (y + 1)

-- the purpose for the anonymous functions (xDelta and yDelta) is so that this funciton can be used to iterate over multiple dimensions possibly
-- makeTargets :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> [Square]
-- possible new name: makeRow
makeTargets 8 y xDelta yDelta array pieces = array
makeTargets x 8 xDelta yDelta array pieces = array 
makeTargets x y xDelta yDelta array pieces = makeTargets (xDelta x) (yDelta y) xDelta yDelta ((makeSquarePiece x y (pieces !! 0)) : array) (drop 1 pieces)

main = do 

  let testArray = []

  let whitePieces = makeTargets 0 0 (\x -> x + 1) (\y -> y) [] 

  let whitePieces = makeTargets 0 0 (\x -> x + 1) (\y -> y) [] (makeStartRow White)
  let blackPieces = makeTargets 0 7 (\x -> x + 1) (\y -> y) [] (makeStartRow Black)
  let whitePawns  = makeTargets 0 2 (\x -> x + 1) (\y -> y) [] (replicate 8 (Piece Pawn White))
  let blackPawns  = makeTargets 0 7 (\x -> x + 1) (\y -> y) [] (replicate 8 (Piece Pawn Black))

  let board = placePiecesRow makeBoard (makeRow 7) 0 7 blackPawns
  let a     = placePiecesRow board (makeRow 2) 0 2 whitePawns
  let b     = placePiecesRow a (makeRow 1) 0 1 whitePieces
  let c     = placePiecesRow b (makeRow 8) 0 8 blackPieces

  printBoard (makeBoard)
  putStrLn ""
  printBoard c
  print "done"












