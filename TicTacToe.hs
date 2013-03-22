module TicTacToe where

import qualified Data.Map as Map
import Safe (readMay)


data Piece = X | O deriving (Eq, Show)

data Error = NonEmpty | OutOfBounds | BadInput
instance Show Error where
  show NonEmpty    = "\nThat square is already taken. Try another move."
  show OutOfBounds = "\nThat square is out of bounds. Enter square from 1 to 3."
  show BadInput    = "\nCouldn't understand input. Try again."

type Position = (Int, Int)

type Board = Map.Map Position Piece


other :: Piece -> Piece
other X = O
other O = X

winningPositions :: [[Position]]
winningPositions = [[(1, 1), (1, 2), (1, 3)], -- first 3: vertical
                    [(2, 1), (2, 2), (2, 3)],
                    [(3, 1), (3, 2), (3, 3)],
                    [(1, 1), (2, 1), (3, 1)], -- next 3: horizontal
                    [(1, 2), (2, 2), (3, 2)],
                    [(1, 3), (2, 3), (3, 3)],
                    [(1, 1), (2, 2), (3, 3)], -- last 2: diagonal
                    [(1, 3), (2, 2), (3, 1)]]  

emptyBoard :: Board
emptyBoard = Map.empty


makeMove :: Board -> String -> Piece -> Either Error Board
makeMove board position piece = let legal = [1, 2, 3] in
  case readMay position :: Maybe (Int, Int) of
    Nothing     -> Left BadInput
    Just (x, y) -> if x `notElem` legal || y `notElem` legal
                   then Left OutOfBounds
                   else case Map.lookup (x, y) board of
                     Nothing -> Right $ Map.insert (x, y) piece board
                     _       -> Left NonEmpty


win :: Board -> Piece -> Bool
win board pieceType = or [threeInARow board pieceType lane
                         | lane <- winningPositions]

threeInARow :: Board -> Piece -> [Position] -> Bool
threeInARow board pieceType lane = 3 == length (filter (== (Just pieceType))
                                          [Map.lookup spot board | spot <- lane])

draw :: Board -> Bool
draw board = (Map.size board == 9) && not (win board X) && not (win board O)

gameOver :: Board -> Maybe Piece
gameOver board
  | win board X = Just X
  | win board O = Just O
  | otherwise   = Nothing



showCell :: Maybe Piece -> String
showCell (Just X) = "X"
showCell (Just O) = "O"
showCell Nothing  = " "

showBoard :: Board -> String
showBoard board =
  let spot = showCell . flip Map.lookup board
  in "   1   2   3 \n"                                                    ++ 
     " 1 " ++ spot (1, 1) ++ " | " ++ spot (2, 1) ++ " | " ++ spot (3, 1) ++ 
     "\n  ---|---|---\n"                                                  ++
     " 2 " ++ spot (1, 2) ++ " | " ++ spot (2, 2) ++ " | " ++ spot (3, 2) ++
     "\n  ---|---|---\n"                                                  ++
     " 3 " ++ spot (1, 3) ++ " | " ++ spot (2, 3) ++ " | " ++ spot (3, 3)


loop :: Board -> Piece -> IO ()
loop board piece = do
  putStrLn $ showBoard board
  case gameOver board of
    Just winner -> putStrLn $ "Player " ++ show winner ++ " wins!"
    Nothing     -> do
      putStrLn $ "\nPlayer " ++ show piece ++ ", move in the form (x, y)."
      input <- getLine
      case makeMove board input piece of
        Left  err      -> putStrLn (show err) >> loop board piece
        Right newBoard -> loop newBoard (other piece)

main :: IO ()
main = loop emptyBoard X

