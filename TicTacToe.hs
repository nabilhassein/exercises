{-# LANGUAGE TupleSections #-}

module Main where

import           Safe (readMay)
import qualified Data.Map as Map


-- data types and their display logic
data Piece = X | O deriving (Eq, Show)

data Error = BadInput | OutOfBounds | NonEmptySquare
instance Show Error where
  show NonEmptySquare = "\nThat square is already taken. Try another move."
  show OutOfBounds    = "\nOut of bounds. Choose a square from 1 to 3."
  show BadInput       = "\nCouldn't understand input. Try again."

type Position = (Int, Int)

type Board = Map.Map Position Piece
showBoard :: Board -> String
showBoard board = let showCell :: Maybe Piece -> String
                      showCell (Just piece) = show piece
                      showCell Nothing      = " "

                      spot :: Position -> String
                      spot = showCell . flip Map.lookup board
  in "\n   1   2   3 "                                                      ++
     "\n 1 " ++ spot (1, 1) ++ " | " ++ spot (2, 1) ++ " | " ++ spot (3, 1) ++
     "\n  ---|---|---"                                                      ++
     "\n 2 " ++ spot (1, 2) ++ " | " ++ spot (2, 2) ++ " | " ++ spot (3, 2) ++
     "\n  ---|---|---"                                                      ++
     "\n 3 " ++ spot (1, 3) ++ " | " ++ spot (2, 3) ++ " | " ++ spot (3, 3) ++
     "\n\n"

-- useful helper functions and variables
other :: Piece -> Piece
other X = O
other O = X

--legal :: [Position]
legal = [1, 2, 3]

emptyBoard :: Board
emptyBoard = Map.empty

--winningPositions :: [(Position, Position, Position)]
winningPositions = [map (1,) legal, -- first three are vertical
                    map (2,) legal,
                    map (3,) legal,
                    map (,1) legal, -- next three are horizontal
                    map (,2) legal,
                    map (,3) legal,
                    map (\x -> (x, x)) legal, -- last two are diagonal
                    [(x, y) | x <- legal, y <- legal, x + y == 4]]


-- game logic
draw :: Board -> Bool
draw board = Map.size board == 9 && not (win board X) && not (win board O)

win :: Board -> Piece -> Bool
win board piece = any (threeInARow board piece) winningPositions

-- retest this after implementing winningPositions
threeInARow :: Board -> Piece -> (Position, Position, Position) -> Bool
threeInARow board piece (x, y, z) = all (== Just piece) [Map.lookup spot board
                                                        | spot <- [x, y, z]]

gameOver :: Board -> Maybe String
gameOver board
  | win board X = Just "\nPlayer X wins!"
  | win board O = Just "\nPlayer O wins!"
  | draw board  = Just "\nCat's Game."
  | otherwise   = Nothing

makeMove :: Position -> Piece -> Board -> Board
makeMove = Map.insert

parseMove :: String -> Board -> Either Error Position
parseMove input board = case readMay input of
  Nothing       -> Left BadInput
  Just position -> if position `notElem` legal
                   then Left OutOfBounds
                   else case Map.lookup position board of
                     Just _  -> Left NonEmptySquare
                     Nothing -> Right position

-- game setup and execution
main :: IO ()
main = loop emptyBoard X

loop :: Board -> Piece -> IO ()
loop board piece = case gameOver board of
  Just string -> putStrLn string
  Nothing     -> do
    putStrLn $ showBoard board ++ show piece ++ ", move in the form (x, y)."
    input <- getLine
    case parseMove input board of
      Left  err      -> print err >> loop board piece
      Right position -> let newBoard = makeMove position piece board
                        in loop newBoard (other piece)
