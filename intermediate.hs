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
  furry _ (EitherLeft (Right b)) = EitherLeft $ Right b

-- exercise 5
instance Fluffy (EitherRight a) where
  furry _ (EitherRight (Left a))  = EitherRight $ Left a
  furry f (EitherRight (Right b)) = EitherRight . Right $ f b



class Misty m where
  unicorn :: a -> m a
  banana  :: (a -> m b) -> m a -> m b
  -- exercise 6
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana $ unicorn . f


-- exercise 7
instance Misty [] where
  unicorn x = [x]
  banana = concatMap

-- exercise 8
instance Misty Maybe where
  unicorn = Just
  banana _ Nothing  = Nothing
  banana f (Just x) = f x

-- exercise 9
instance Misty ((->) t) where
  unicorn = const
  banana f g x = f (g x) x

-- exercise 10
instance Misty (EitherLeft b) where
  unicorn = EitherLeft . Left
  banana f (EitherLeft (Left a))  = f a
  banana _ (EitherLeft (Right b)) = EitherLeft $ Right b

-- exercise 11
instance Misty (EitherRight a) where
  unicorn = EitherRight . Right
  banana _ (EitherRight (Left a))  = EitherRight $ Left a
  banana f (EitherRight (Right b)) = f b


-- exercise 12
jellybean :: Misty m => m (m a) -> m a
jellybean = banana id

-- exercise 13
apple :: Misty m => m a -> m (a -> b) -> m b
apple x f = undefined

-- exercise 14
moppy :: Misty m => [a] -> (a -> m b) -> m [b]
moppy = undefined

-- exercise 15
sausage :: Misty m => [m a] -> m [a]
sausage []     = unicorn []
sausage (x:xs) = undefined
