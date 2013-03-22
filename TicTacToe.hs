module TicTacToe where

import qualified Data.Map as Map
import Safe (readMay)
import Control.Monad (when)

data Piece = X | O deriving (Eq, Show)

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


makeMove :: Board -> Piece -> IO Board
makeMove board piece = do
  putStrLn $ "Player " ++ show piece ++ " to move."
  position <- getMove
  case Map.lookup position board of
    Nothing -> return $ Map.insert position piece board
    _       -> putStrLn "Choose an empty square." >> makeMove board piece


getMove :: IO Position
getMove = do
  putStrLn "Make your move in the form (x, y)"
  input <- getLine
  let pos   = readMay input :: Maybe (Int, Int)
      legal = [1, 2, 3]
  case pos of
    Just (x, y) -> if x `elem` legal && y `elem` legal
                   then return (x, y)
                   else putStrLn "Both coordinates should be from 1 to 3."
                        >> getMove
    _           -> putStrLn "Couldn't understand input. Try again." >> getMove


win :: Board -> Piece -> Bool
win board pieceType = or [threeInARow board pieceType lane
                         | lane <- winningPositions]

threeInARow :: Board -> Piece -> [Position] -> Bool
threeInARow board pieceType lane = 3 == length (filter (== (Just pieceType))
                                          [Map.lookup spot board | spot <- lane])

draw :: Board -> Bool
draw board = full && not (win board X) && not (win board O)
  where full   = length values == 9
        values = map snd $ Map.toList board

gameOver :: Board -> IO Bool
gameOver board = do
  let x = win board X
      o = win board O
      d = draw board
  when x $ putStrLn "Player X wins!"
  when o $ putStrLn "Player O wins!"
  when d $ putStrLn "Cat's game!"
  return $ x || o || d

showCell :: Maybe Piece -> String
showCell (Just X) = "X"
showCell (Just O) = "O"
showCell Nothing  = "."

showBoard :: Board -> String
showBoard board =
  let spot = showCell . flip Map.lookup board
  in "   1   2   3 \n" ++ 
     " 1 " ++ spot (1, 1) ++ " | " ++ spot (2, 1) ++ " | " ++ spot (3, 1) ++ 
     "\n  ---|---|---\n"                                                  ++
     " 2 " ++ spot (1, 2) ++ " | " ++ spot (2, 2) ++ " | " ++ spot (3, 2) ++
     "\n  ---|---|---\n"                                                  ++
     " 3 " ++ spot (1, 3) ++ " | " ++ spot (2, 3) ++ " | " ++ spot (3, 3) ++
     "\n"

loop :: Board -> Piece -> IO ()
loop board piece = do
  putStrLn $ showBoard board
  done <- gameOver board
  if not done
    then do newBoard <- makeMove board piece
            loop newBoard (other piece)
    else return ()

main :: IO ()
main = loop emptyBoard X

