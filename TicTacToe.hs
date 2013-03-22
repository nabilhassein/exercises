{-# LANGUAGE TupleSections #-}

module TicTacToe where

import qualified Data.Map as Map
import Control.Monad (guard, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Maybe (MaybeT, runMaybeT)


data Piece = X | O deriving (Eq, Show)

other :: Piece -> Piece
other X = O
other O = X

-- is there a better way to represent this?
data Legal = One | Two | Three deriving (Eq, Ord, Show)

convert :: Int -> Maybe Legal
convert 1 = Just One
convert 2 = Just Two
convert 3 = Just Three
convert _ = Nothing

type Position = (Legal, Legal)

-- could probably employ a bit of cleverness here instead
winningPositions :: [[Position]]
winningPositions =
  [[(One  , One)  , (One  , Two)  , (One  , Three)], -- first 3: vertical
   [(Two  , One)  , (Two  , Two)  , (Two  , Three)],
   [(Three, One)  , (Three, Two)  , (Three, Three)],
   [(One  , One)  , (Two  , One)  , (Three, One)]  , -- next 3: horizontal
   [(One  , Two)  , (Two  , Two)  , (Three, Two)]  ,
   [(One  , Three), (Two  , Three), (Three, Three)],
   [(One  , One)  , (Two  , Two)  , (Three, Three)], -- last 2: diagonal
   [(One  , Three), (Two  , Two)  , (Three, One)]]  


type Board = Map.Map Position Piece

emptyBoard :: Board
emptyBoard = Map.empty


makeMove :: Board -> Position -> Piece -> Maybe Board
makeMove board position pieceType = case Map.lookup position board of
  Nothing -> Just $ Map.insert position pieceType board
  _       -> Nothing  -- can only insert an X or O at an empty square


getMove :: MaybeT IO Position
getMove = let puts = liftIO . putStrLn in do
  puts "Make your move in the form (x, y)"
  input <- liftIO getLine
  let (x, y) = read input :: (Int, Int) -- TODO: refactor using safe read
  case (convert x, convert y) of
    (Just a, Just b) -> return (a, b)
    _                -> puts "Illegal move. Try again." >> getMove


threeInARow :: Board -> Piece -> [Position] -> Bool
threeInARow board pieceType lane = 3 == length (filter (== (Just pieceType))
                                          [Map.lookup spot board | spot <- lane])

win :: Board -> Piece -> Bool
win board pieceType = or [threeInARow board pieceType lane | lane <- winningPositions]

  
draw :: Board -> Bool
draw board = full && not (win board X) && not (win board O)
  where full   = length values == 9
        values = map snd $ Map.toList board


gameOver :: Board -> Bool
gameOver board = win board X || win board O || draw board

-- for testing win, draw, etc.
test :: Board
test = Map.fromList [((One, One), X), ((Two, One), O), ((Three, One), O),
                     ((One, Two), O), ((Two, Two), X), ((Three, Two), O),
                     ((One, Three), X), ((Two, Three), O), ((Three, Three), X)]
                         

showCell :: Maybe Piece -> String
showCell (Just X) = "X"
showCell (Just O) = "O"
showCell Nothing  = "."

showBoard :: Board -> String
showBoard board =
  let spot = showCell . flip Map.lookup board
  in " " ++ spot (One, One) ++ " | " ++ spot (Two, One) ++ " | " ++ spot (Three, One)
     ++ "\n---|---|---\n" ++
     " " ++ spot (One, Two) ++ " | " ++ spot (Two, Two) ++ " | " ++ spot (Three, Two)
     ++ "\n---|---|---\n" ++
     " " ++ spot (One, Three) ++ " | " ++ spot (Two, Three) ++ " | " ++ spot (Three, Three)


loop :: Board -> Piece -> MaybeT IO ()
loop board piece = let puts = liftIO . putStrLn in do
  puts $ showBoard board ++ "\n"
  when (win board X) (puts "Player X wins!")
  when (win board O) (puts "Player O wins!")
  when (draw board) (puts "Cat's game!")
  guard . not $ gameOver board -- TODO: real victory/draw message w/o "Nothing"
  puts $ "Player " ++ show piece ++ " to move."
  position <- getMove
  case makeMove board position piece of
    Just newBoard -> loop newBoard (other piece)
    Nothing       -> (puts "Illegal move. Try again.") >> loop board piece

main :: IO (Maybe ())
main = runMaybeT $ loop emptyBoard X

