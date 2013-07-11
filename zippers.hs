-- based on http://learnyouahaskell.com/zippers

module Zippers where


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Direction = L | R deriving (Show)
type Directions = [Direction]

-- the obvious generalization of the original code
alterTree :: a -> Directions -> Tree a -> Tree a
alterTree a (L:ds) (Node x l r) = Node x (alterTree a ds l) r
alterTree a (R:ds) (Node x l r) = Node x l (alterTree a ds r)
alterTree a []     (Node _ l r) = Node a l r

changeToP' = alterTree 'P'

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x


freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )
