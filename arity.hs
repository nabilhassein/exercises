{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

class Arity f where
  arity :: f -> Int

instance Arity x where
  arity _ = 0

instance Arity f => Arity ((->) a f) where
  arity f = 1 + arity (f undefined)



-- based on the discussion here:
-- http://stackoverflow.com/questions/8371499/how-does-incoherentinstances-work