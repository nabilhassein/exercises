{-# LANGUAGE NoImplicitPrelude #-}

module MyPrelude where

import qualified Prelude as P

data Maybe a = Nothing | Just a
             deriving (P.Eq, P.Show, P.Read, P.Ord)

data Either e a = Left e | Right a
                deriving (P.Eq, P.Show, P.Read, P.Ord)

data Bool = False | True
           deriving (P.Eq, P.Show, P.Read, P.Ord, P.Bounded, P.Enum)

type Int = P.Int

undefined :: a
undefined = let x = x in x


id :: a -> a
id x = x

infixr 1 $
($) :: (a -> b) -> a -> b
f $ x = f x

const :: a -> b -> a
const x y = x

(.) :: (b -> c) -> (a -> b) -> a -> c
g . f = \x -> g (f x)
              
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x


not :: Bool -> Bool
not True  = False
not False = True


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ xs [] = []
zipWith _ [] ys = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x : replicate (n P.- 1) x

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

concat :: [[a]] -> [a]
concat []     = []
concat [[]]   = []
concat (x:xs) = x P.++ concat xs