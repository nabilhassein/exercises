{-# LANGUAGE TupleSections #-}

module TicTacToe where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Control.Monad.Maybe

data Piece = X | O deriving (Eq, Show)

other :: Piece -> Piece
other X = O
other O = X


type Position = (Int, Int)

-- could probably employ a bit of cleverness here instead
winningPositions :: [[Position]]
winningPositions = [[(1, 1), (1, 2), (1, 3)],
                    [(2, 1), (2, 2), (2, 3)],
                    [(3, 1), (3, 2), (3, 3)], -- first 3 are vertical
                    [(1, 1), (2, 1), (3, 1)],
                    [(1, 2), (2, 2), (3, 2)],
                    [(1, 3), (2, 3), (3, 3)], -- next 3 are horizontal
                    [(1, 1), (2, 2), (3, 3)],
                    [(1, 3), (2, 2), (3, 1)]] -- last 2 are diagonal


type Board = Map.Map Position (Maybe Piece)
-- Just Piece means already played there; Nothing means empty square

emptyBoard :: Board
emptyBoard = Map.fromList $ map (, Nothing) [(1, 1), (2, 1), (3, 1),
                                             (1, 2), (2, 2), (3, 2),
                                             (1, 3), (2, 3), (3, 3)]



move :: Board -> Position -> Piece -> Maybe Board
move board position pieceType = case Map.lookup position board of
  Nothing -> Just $ Map.insert position (Just pieceType) board
  _       -> Nothing  -- can only insert X or O at empty square


getMove :: MaybeT IO Position
getMove = do
  liftIO $ putStrLn "Make your move in the form (x, y)"
  input <- liftIO getLine
  let position = read input :: Position
  guard $ position `elem` Map.keys emptyBoard -- check if a legal square
  return position


check :: Board -> Piece -> [Position] -> Bool
check board pieceType lane = 3 == length (filter (== (Just pieceType))
                                          [fromJust $ Map.lookup spot board
                                          | spot <- lane])

win :: Board -> Piece -> Bool
win board pieceType = or [check board pieceType lane | lane <- winningPositions]

  
draw :: Board -> Bool
draw board = full && not (win board X) && not (win board O)
  where full   = Nothing `notElem` values
        values = map snd $ Map.toList board


-- for testing win, draw, etc.
test :: Board
test = Map.fromList [((1, 1), Just X), ((2, 1), Just O), ((3, 1), Just O),
                     ((1, 2), Just O), ((2, 2), Just X), ((3, 2), Just O),
                     ((1, 3), Just X), ((2, 3), Just O), ((3, 3), Just X)]
                         


showPiece :: Maybe Piece -> String
showPiece (Just X) = "X"
showPiece (Just O) = "O"
showPiece Nothing  = "."

showBoard :: Board -> String
showBoard board =
  let spot = showPiece . fromJust . flip Map.lookup board
  in " " ++ spot (1, 1) ++ " | " ++ spot (2, 1) ++ " | " ++ spot (3, 1)
     ++ "\n---|---|---\n" ++
     " " ++ spot (1, 2) ++ " | " ++ spot (2, 2) ++ " | " ++ spot (3, 2)
     ++ "\n---|---|---\n" ++
     " " ++ spot (1, 3) ++ " | " ++ spot (2, 3) ++ " | " ++ spot (3, 3)


loop :: Board -> Piece -> MaybeT IO ()
loop board piece = let puts = liftIO . putStrLn in do
  puts $ showBoard board
  puts $ "Player " ++ show piece ++ " to move."
  position <- getMove
  board' <- return $ move board position piece
  case board' of
    Just newBoard -> loop newBoard (other piece)
    Nothing       -> (puts $ "Bad input. Try again.") >> loop board piece

main :: IO (Maybe ())
main = runMaybeT $ loop emptyBoard X
  