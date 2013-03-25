{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as Map
import Safe (readMay)
import Data.Char (toUpper, toLower)

-- data types and their display logic
data Piece = X | O deriving (Eq, Show, Read)

data Error = NonEmptySquare | OutOfBounds | BadInput
instance Show Error where
  show NonEmptySquare = "\nThat square is already taken. Try another move."
  show OutOfBounds    = "\nOut of bounds. Choose a square from 1 to 3."
  show BadInput       = "\nCouldn't understand input. Try again."

type Position = (Int, Int)

type Board = Map.Map Position Piece
showBoard :: Board -> String
showBoard board = let showCell :: Maybe Piece -> String
                      showCell (Just X) = "X"
                      showCell (Just O) = "O"
                      showCell Nothing  = " "

                      spot :: Position -> String
                      spot = showCell . flip Map.lookup board
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
threeInARow board piece lane = all (== Just piece) [Map.lookup spot board
                                                   | spot <- lane]

draw :: Board -> Bool
draw board = Map.size board == 9 && not (win board X) && not (win board O)

gameOver :: Board -> Maybe String
gameOver board
  | win board X = Just "\nPlayer X wins!"
  | win board O = Just "\nPlayer O wins!"
  | draw board  = Just "\nCat's Game."
  | otherwise   = Nothing

makeMove :: Board -> Piece -> Maybe Position -> Either Error Board
makeMove _      _    Nothing       = Left BadInput
makeMove board piece (Just (x, y)) =
  if x `notElem` legal || y `notElem` legal
  then Left OutOfBounds
  else case Map.lookup (x, y) board of
    Just _  -> Left NonEmptySquare
    Nothing -> Right $ Map.insert (x, y) piece board


-- game setup and execution; only these functions perform I/O
main :: IO ()
main = do
  putStrLn "Enter 'human' without quotes to play for two players.\n\
           \Enter anything else to face the unbeatable machine."
  opponent <- getLine
  case map toLower opponent of
    "human" -> twoPlayers emptyBoard X
    _       -> startSoloGame


twoPlayers :: Board -> Piece -> IO ()
twoPlayers board piece = do
  putStrLn $ showBoard board
  case gameOver board of
    Just string -> putStrLn string
    Nothing     -> do
      putStrLn $ "\nPlayer " ++ show piece ++ ", move in the form (x, y)."
      input <- getLine
      let position = readMay input :: Maybe Position
      case makeMove board piece position of
        Left  err      -> putStrLn (show err) >> twoPlayers board piece
        Right newBoard -> twoPlayers newBoard (other piece)


startSoloGame :: IO ()
startSoloGame = choosePiece >>= onePlayer emptyBoard

choosePiece :: IO Piece
choosePiece = do
  putStrLn "Enter the name of the piece you want to play."
  piece:_ <- getLine
  -- explicitly ignore all but first char so that the recursive call doesn't
  -- confusedly treat each part of input string as separate, like getChar does
  case toUpper piece of
    'X' -> return X
    'O' -> return O
    _   -> putStrLn "Bad input. Only enter X or O." >> choosePiece

onePlayer :: Board -> Piece -> IO ()
onePlayer _ _ = putStrLn "onePlayer" -- TODO

