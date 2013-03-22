module TicTacToe where

import qualified Data.Map as Map
import Safe (readMay)
import Control.Monad (guard, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Maybe (MaybeT, runMaybeT)

data Piece = X | O deriving (Eq, Show)

other :: Piece -> Piece
other X = O
other O = X

type Position = (Int, Int)

winningPositions :: [[Position]]
winningPositions = [[(1, 1), (1, 2), (1, 3)], -- first 3: vertical
                    [(2, 1), (2, 2), (2, 3)],
                    [(3, 1), (3, 2), (3, 3)],
                    [(1, 1), (2, 1), (3, 1)], -- next 3: horizontal
                    [(1, 2), (2, 2), (3, 2)],
                    [(1, 3), (2, 3), (3, 3)],
                    [(1, 1), (2, 2), (3, 3)], -- last 2: diagonal
                    [(1, 3), (2, 2), (3, 1)]]  


type Board = Map.Map Position Piece

emptyBoard :: Board
emptyBoard = Map.empty

puts :: String -> MaybeT IO ()
puts = liftIO . putStrLn

makeMove :: Board -> Piece -> MaybeT IO Board
makeMove board piece = do
  position <- getMove
  case Map.lookup position board of
    Nothing -> return $ Map.insert position piece board
    _       -> puts "Choose an empty square." >> makeMove board piece


getMove :: MaybeT IO Position
getMove = do
  puts "Make your move in the form (x, y)"
  input <- liftIO getLine
  let pos = readMay input :: Maybe (Int, Int)
  case pos of
    Just (x, y) -> if x `elem` [1, 2, 3] && y `elem` [1, 2, 3]
                   then return (x, y)
                   else puts "Both coordinates should be from 1 to 3." >> getMove
    _           -> puts "Illegal input. Try again." >> getMove


threeInARow :: Board -> Piece -> [Position] -> Bool
threeInARow board pieceType lane = 3 == length (filter (== (Just pieceType))
                                          [Map.lookup spot board | spot <- lane])

win :: Board -> Piece -> Bool
win board pieceType = or [threeInARow board pieceType lane
                         | lane <- winningPositions]

  
draw :: Board -> Bool
draw board = full && not (win board X) && not (win board O)
  where full   = length values == 9
        values = map snd $ Map.toList board


gameOver :: Board -> Bool
gameOver board = win board X || win board O || draw board

-- for testing win, draw, etc.
test :: Board
test = Map.fromList [((1, 1), X), ((2, 1), O), ((3, 1), O),
                     ((1, 2), O), ((2, 2), X), ((3, 2), O),
                     ((1, 3), X), ((2, 3), O), ((3, 3), X)]
                         

showCell :: Maybe Piece -> String
showCell (Just X) = "X"
showCell (Just O) = "O"
showCell Nothing  = "."

showBoard :: Board -> String
showBoard board =
  let spot = showCell . flip Map.lookup board
  in "   1   2   3 \n" ++ 
     " 1 " ++ spot (1, 1) ++ " | " ++ spot (2, 1) ++ " | " ++ spot (3, 1)
     ++ "\n  ---|---|---\n" ++
     " 2 " ++ spot (1, 2) ++ " | " ++ spot (2, 2) ++ " | " ++ spot (3, 2)
     ++ "\n  ---|---|---\n" ++
     " 3 " ++ spot (1, 3) ++ " | " ++ spot (2, 3) ++ " | " ++ spot (3, 3)


loop :: Board -> Piece -> MaybeT IO ()
loop board piece = do
  puts $ showBoard board ++ "\n"
  when (win board X) (puts "Player X wins!")
  when (win board O) (puts "Player O wins!")
  when (draw board) (puts "Cat's game!")
  guard . not $ gameOver board -- TODO: real victory/draw message w/o "Nothing"
  puts $ "Player " ++ show piece ++ " to move."
  newBoard <- makeMove board piece
  loop newBoard (other piece)

main :: IO (Maybe ())
main = runMaybeT $ loop emptyBoard X

