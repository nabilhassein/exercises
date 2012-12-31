-- 20 intermediate haskell problems
-- http://blog.tmorris.net/20-intermediate-haskell-exercises/

class Fluffy f where
  furry :: (a -> b) -> f a -> f b


-- exercise 1
instance Fluffy [] where
  furry = map

-- exercise 2
instance Fluffy Maybe where
  furry _ Nothing  = Nothing
  furry f (Just x) = Just $ f x

-- exercise 3
instance Fluffy ((->) t) where
  furry = (.)

newtype EitherLeft b a  = EitherLeft  (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- exercise 4
instance Fluffy (EitherLeft b) where
  furry f (EitherLeft (Left a))  = EitherLeft . Left $ f a
  furry _ (EitherLeft (Right b)) = EitherLeft . Right $ b

-- exercise 5
instance Fluffy (EitherRight a) where
  furry _ (EitherRight (Left a))  = EitherRight . Left $ a
  furry f (EitherRight (Right b)) = EitherRight . Right $ f b



class Misty m where
  banana  :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- exercise 6
  furry' :: (a -> b) -> m a -> m b
  furry' f x = error "todo"