{-# Language NoMonomorphismRestriction #-}

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Ratio
import Data.Function (on)
import System.IO

--exercise 2.5.1
ttRatio :: [String] -> Rational
ttRatio tokens = Set.size (Set.fromList tokens) %% length tokens
  where (%%) = (%) `on` fromIntegral


wordList :: Ord b => [b] -> Set.Set b
wordList = foldl (flip Set.insert) Set.empty

countElem :: Ord k => Map.Map k Int -> k -> Map.Map k Int
countElem m e = Map.insertWith (+) e 1 m

frequencyList :: Ord k => [k] -> Map.Map k Int
frequencyList = foldl countElem Map.empty

main :: IO ()
main = do
  c <- hGetContents =<< openFile "nlpwp-data/brown.txt" ReadMode
  putStrLn . show $ Map.lookup "the" (frequencyList (words c))