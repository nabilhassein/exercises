-- based loosely on http://learnyouahaskell.com/zippers

module Zippers where

import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as BL


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Zipper a = (Tree a, [Crumb a])

(-:) :: a -> (a -> b) -> b
x -: f = f x

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: Zipper a -> Zipper a
goUp (t, (LeftCrumb  x r) : bs) = (Node x t r, bs)
goUp (t, (RightCrumb x l) : bs) = (Node x l t, bs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f ((Node x l r), bs) = (Node (f x) l r, bs)
modify _ (Empty,        bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z      = topMost (goUp z)


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
