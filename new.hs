module Main where

-- DATA TYPE SECTION
data Color = Black | White deriving Show
data PieceName = King | Queen | Bishop | Knight | Rook | Pawn | Null deriving (Show)
data SquareState = Occupied | Empty

data Piece = Piece PieceName Color
data Square = Square Int Int Color Piece SquareState
data Point = Point Int Int
pointGetX (Point x _) = x
pointGetY (Point _ y) = y

squareGetY :: Square -> Int
squareGetY (Square _ y _ _ _) = y
squareGetX :: Square -> Int
squareGetX (Square x _ _ _ _) = x
squareGetP :: Square -> Piece
squareGetP (Square _ _ _ piece _) = piece
pieceGetName (Piece name _) = name
squareGetPN (Square _ _ _ piece _) = (pieceGetName piece)

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
makeOccupiedSquare x y piece = Square x y (getColor (y + x)) piece Occupied

makeRow y = [makeSquare x y | x <- [1..8]]
makePawnRow y color = [makeOccupiedSquare x y (Piece Pawn color) | x <- [1..8]]

getColor :: Int -> Color
getColor x = if (x `mod` 2) == 0 then Black else White

makeBoardHelper board 0 = board
makeBoardHelper board y = makeBoardHelper ((makeRow y) : board) (y - 1)
makeBoard = makeBoardHelper [] 8 

makePieces color = [(Piece Pawn color), (Piece Rook color), (Piece Knight color), (Piece Bishop color), (Piece Queen color), (Piece King White)] 
makeStartRow :: Color -> [Piece]
makeStartRow c = [(Piece Rook c), (Piece Knight c), (Piece Bishop c), (Piece Queen c), (Piece King c), (Piece Bishop c), (Piece Knight c), (Piece Rook c)]

-- MUTATION FUNCTIONS
mutateRowHelper :: Square -> Square -> Square
mutateRowHelper ap new = if (new == ap) then new else ap
mutateRow :: Square -> [Square] -> [Square]
mutateRow newSquare row = map (\x -> mutateRowHelper x newSquare) row

mutateBoard :: Square -> [[Square]] -> [[Square]]
mutateBoardHelper currRow newSquare = if ((squareGetY (currRow !! 0)) == (squareGetY newSquare)) then (mutateRow newSquare currRow) else currRow
mutateBoard newSquare board = map (\currRow -> mutateBoardHelper currRow newSquare) board
--END FUNCTIONS

place :: [Square] -> [[Square]] -> [[Square]]
place [] board = board
place pieces board = place (drop 1 pieces) (mutateBoard (pieces !! 0) board)

makeTargets :: [(Point, Piece)] -> [Square]
makeTargets zippedPoints = map (\(point, piece) -> makeOccupiedSquare (pointGetX point) (pointGetY point) piece) zippedPoints

-- appendArray :: [a] -> [a] -> [a]
-- appendArray arrOne arrTwo = map (\x -> x : arrTwo) arrOne

main = do 
  let board = makeBoard
  let pieces = [(Piece Pawn White), (Piece Pawn White), (Piece Pawn White), (Piece Pawn White), (Piece Pawn White), (Piece Pawn White), (Piece Pawn White), (Piece Pawn White), (Piece Queen White)]
  let points = [(Point 1 2), (Point 2 2), (Point 3 2), (Point 4 2), (Point 5 2), (Point 6 2), (Point 7 2), (Point 8 2), (Point 5 5)]

  let targets = makeTargets (zip points pieces)
  let a = place targets board

  printBoard a
  print "done"












