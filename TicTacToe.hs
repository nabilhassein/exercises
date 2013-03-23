module TicTacToe where

import qualified Data.Map as Map
import Safe (readMay)


-- data types and their display logic
data Piece = X | O deriving (Eq, Show)

data Error = NonEmpty | OutOfBounds | BadInput
instance Show Error where
  show NonEmpty    = "\nThat square is already taken. Try another move."
  show OutOfBounds = "\nOut of bounds. Choose a square from 1 to 3."
  show BadInput    = "\nCouldn't understand input. Try again."

type Position = (Int, Int)

type Board = Map.Map Position Piece
showBoard :: Board -> String
showBoard board =
  let spot = showCell . flip Map.lookup board
      showCell (Just X) = "X"
      showCell (Just O) = "O"
      showCell Nothing  = " "
  in "\n   1   2   3 "                                                      ++ 
     "\n 1 " ++ spot (1, 1) ++ " | " ++ spot (2, 1) ++ " | " ++ spot (3, 1) ++ 
     "\n  ---|---|---"                                                      ++
     "\n 2 " ++ spot (1, 2) ++ " | " ++ spot (2, 2) ++ " | " ++ spot (3, 2) ++
     "\n  ---|---|---"                                                      ++
     "\n 3 " ++ spot (1, 3) ++ " | " ++ spot (2, 3) ++ " | " ++ spot (3, 3)


-- useful helper functions and variables
other :: Piece -> Piece
other X = O
other O = X

legal :: [Int]
legal = [1, 2, 3]

emptyBoard :: Board
emptyBoard = Map.empty

winningPositions :: [[Position]]
winningPositions = [map (1,) legal, -- first three are vertical
                    map (2,) legal,
                    map (3,) legal,
                    map (,1) legal, -- next three are horizontal
                    map (,2) legal,
                    map (,3) legal,
                    map (\x -> (x, x)) legal, -- last two are diagonal
                    [(x, y) | x <- legal, y <- legal, x + y == 4]]


-- game logic
win :: Board -> Piece -> Bool
win board piece = any (threeInARow board piece) winningPositions

threeInARow :: Board -> Piece -> [Position] -> Bool
threeInARow board piece lane = all (== (Just piece))
                               [Map.lookup spot board | spot <- lane]

draw :: Board -> Bool
draw board = (Map.size board == 9) && not (win board X) && not (win board O)

gameOver :: Board -> Maybe String
gameOver board
  | win board X = Just "\nPlayer X wins!"
  | win board O = Just "\nPlayer O wins!"
  | draw board  = Just "\nCat's Game."
  | otherwise   = Nothing

makeMove :: Board -> String -> Piece -> Either Error Board
makeMove board position piece =
  case readMay position :: Maybe (Int, Int) of
    Nothing     -> Left BadInput
    Just (x, y) -> if x `notElem` legal || y `notElem` legal
                   then Left OutOfBounds
                   else case Map.lookup (x, y) board of
                     Nothing -> Right $ Map.insert (x, y) piece board
                     _       -> Left NonEmpty

loop :: Board -> Piece -> IO ()
loop board piece = do
  putStrLn $ showBoard board
  case gameOver board of
    Just string -> putStrLn string
    Nothing     -> do
      putStrLn $ "\nPlayer " ++ show piece ++ ", move in the form (x, y)."
      input <- getLine
      case makeMove board input piece of
        Left  err      -> putStrLn (show err) >> loop board piece
        Right newBoard -> loop newBoard (other piece)

main :: IO ()
main = loop emptyBoard X

