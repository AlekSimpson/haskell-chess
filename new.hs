module Main where
import Text.Printf

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

makeRow y = [makeSquare 1 y, makeSquare 2 y, makeSquare 3 y, makeSquare 4 y, makeSquare 5 y, makeSquare 6 y, makeSquare 7 y, makeSquare 8 y] 

getColor :: Int -> Color
getColor x = if (x `mod` 2) == 0 then White else Black

makeBoardHelper board 0 = board
makeBoardHelper board y = makeBoardHelper ((makeRow y) : board) (y - 1)
makeBoard = makeBoardHelper [] 8 

makePieces color = [(Piece Pawn color), (Piece Rook color), (Piece Knight color), (Piece Bishop color), (Piece Queen color), (Piece King White)] 

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
placePawns :: [[Square]] -> [Square] -> Int -> Int -> Color -> [[Square]]
placePawns board targets 8 y color = board
placePawns board targets x y color = 
  placePawns (mutateBoard (targets !! 0) (makeSquarePiece x y (Piece Pawn color)) board) (drop 1 targets) (x + 1) y color

main = do 
  let whitePieces      = makePieces White
  let blackPiece       = makePieces Black
  -- let board            = makeBoard
  --let targetPointOne   = makeSquare 1 1 
  --let targetPointTwo   = makeSquare 2 1 
  --let targetPointThree = makeSquare 3 1 
  --let targetPointFour  = makeSquare 4 1

  --let bRook   = makeSquarePiece 1 1 (blackPiece !! 1)
  --let bKnight = makeSquarePiece 2 1 (blackPiece !! 2)
  --let bBishop = makeSquarePiece 3 1 (blackPiece !! 3)
  --let bQueen  = makeSquarePiece 4 1 (blackPiece !! 4)

  --let one   = mutateBoard targetPointOne bRook board
  --let two   = mutateBoard targetPointTwo bKnight one
  --let three = mutateBoard targetPointThree bBishop two
  --let four  = mutateBoard targetPointFour bQueen three
  let targets = [(makeSquare 0 7), (makeSquare 1 7), (makeSquare 2 7), (makeSquare 3 7), (makeSquare 4 7), (makeSquare 5 7), (makeSquare 6 7), (makeSquare 7 7)]

  putStrLn (show (length targets))
  let test = drop 1 targets
  putStrLn (show (length test)) 
  -- let board = placePawns makeBoard targets 0 7 Black
  
  -- printBoard board
  print "done"












