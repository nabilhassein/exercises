import Prelude hiding (Functor, fmap, Monad, (>>=), (>>), (=<<), sequence, sequence_, mapM, mapM_)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor f) => Pointed f where
  pure :: a -> f a

class (Pointed f) => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b

class (Applicative m) => Monad m where
  join :: m (m a) -> m a
  join x = x >>= id
  (>>=) :: m a -> (a -> m b) -> m b
  x >>= f = join (fmap f x)
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty


instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just $ f x

instance Pointed Maybe where
  pure = Just

instance Applicative Maybe where
  Just f <*> Just x = Just $ f x
  _      <*> _      = Nothing

instance Monad Maybe where
  Just x >>= f  = f x
  Nothing >>= _ = Nothing


instance Functor (Either e) where
  fmap _ (Left e)  = Left e
  fmap f (Right x) = Right $ f x

instance Pointed (Either e) where
  pure = Right

instance Applicative (Either e) where
  Right f <*> Right x = Right $ f x

instance Monad (Either e) where
  Right x >>= f = f x
  Left e >>= _  = Left e


instance Functor [] where
  fmap = map

instance Pointed [] where
  pure x = [x]

instance Applicative [] where
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Monad [] where
  join = concat


newtype ZipList a = ZipList {getZipList :: [a]}
instance Functor ZipList where
  fmap f = ZipList . map f . getZipList

instance Pointed ZipList where
  pure x = ZipList [x]

instance Applicative ZipList where
  ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs


instance Functor ((,) e) where
  fmap f (e, x) = (e, f x)

instance (Monoid e) => Pointed ((,) e) where
  pure x = (mempty, x)

instance (Monoid e) => Applicative ((,) e) where
  (e1, f) <*> (e2, x) = (e1 `mappend` e2, f x)

instance (Monoid e) => Monad ((,) e) where
  join (e1, (e2, x)) = (e1 `mappend` e2, x)


instance Functor ((->) e) where
  fmap = (.)

instance Pointed ((->) e) where
  pure = const

instance Applicative ((->) e) where
  f <*> g = \x -> f x (g x)

instance Monad ((->) e) where
  join f x = f x x


sequence :: Monad m => [m a] -> m [a]
sequence [] = pure []
sequence (x:xs) =
  x >>= \a ->
  sequence xs >>= \as ->
  pure $ a:as

sequence_ :: Monad m => [m a] -> m ()
sequence_ xs = sequence xs >> pure ()

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n = sequence . replicate n

when :: (Monad m) => Bool -> m () -> m ()
when p x = if p then x else pure ()

unless :: (Monad m) => Bool -> m () -> m ()
unless = when . not

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM

forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

