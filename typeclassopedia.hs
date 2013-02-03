{-# LANGUAGE NoImplicitPrelude #-}

--import Prelude hiding (Functor, fmap, Monad, return, (>>=), (>>), (=<<), sequence, sequence_, mapM, mapM_)
import MyPrelude as Prelude

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor f) => Pointed f where
  pure :: a -> f a


class (Pointed f) => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: (Applicative f) => f a -> f b -> f b
  (*>) = liftA2 $ flip const

  (<*) :: (Applicative f) => f a -> f b -> f a
  (<*) = liftA2 const


class (Applicative m) => Monad m where
  return :: a -> m a
  return = pure

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
  Just x  >>= f = f x
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
  Left e  >>= _ = Left e


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


-- applicative utilities

-- synonym for fmap
(<$>) :: (Applicative f) => (a -> b) -> f a -> f b
f <$> x = pure f <*> x

(<$) :: (Applicative f) => a -> f b -> f a
(<$) = fmap . const

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)

-- another synonym for fmap
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA = (<$>)

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f x y z = f <$> x <*> y <*> z



-- monad utilities

-- exact synonym for fmap
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f x = x >>= (return . f)

--exact synonym for (<*>)
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap f x =
  f >>= \g ->
  x >>= \y ->
  return $ g y

sequence :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (x:xs) =
  x >>= \a ->
  sequence xs >>= \as ->
  return $ a:as

--TODO: figure out NoImplicitPrelude hack so you can do this, etc.
-- sequence (x:xs) = do
--   a <- x
--   as <- sequence xs
--   return $ a:as

sequence_ :: Monad m => [m a] -> m ()
sequence_ xs = sequence xs >> return ()

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n = sequence . replicate n

when :: (Monad m) => Bool -> m () -> m ()
when True  x = x
when False _ = return ()

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

-- TODO: alternative

