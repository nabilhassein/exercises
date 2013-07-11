module Typeclassopedia where


import Prelude hiding
  (
    Functor, fmap
  , Monad, return, (>>=), (>>), (=<<)
  , sequence, sequence_, mapM, mapM_
  )


class Functor f where
  fmap :: (a -> b) -> f a -> f b


class Functor f => Pointed f where
  pure :: a -> f a


class Pointed f => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: (Applicative f) => f a -> f b -> f b
  (*>) = liftA2 $ flip const

  (<*) :: (Applicative f) => f a -> f b -> f a
  (<*) = liftA2 const


class Applicative f => Alternative f where
  empty :: f a

  (<|>) :: f a -> f a -> f a

  some :: f a -> f [a]
  some v = liftA2 (:) v (many v)

  many :: f a -> f [a]
  many v = some v <|> pure []


class Applicative m => Monad m where
  return :: a -> m a
  return = pure

  join :: m (m a) -> m a
  join x = x >>= id

  (>>=) :: m a -> (a -> m b) -> m b
  x >>= f = join $ fmap f x

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y


class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a


class Monad m => MonadFix m where
  mfix :: (a -> m a) -> m a


class MonadTrans t where
  lift :: Monad m => m a -> t m a


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

instance MonadPlus Maybe where
  mzero = Nothing
  Nothing `mplus` b = b
  Just x  `mplus` _ = Just x


instance Functor (Either e) where
  fmap _ (Left  e) = Left e
  fmap f (Right x) = Right $ f x

instance Pointed (Either e) where
  pure = Right

instance Applicative (Either e) where
  Right f <*> Right x = Right $ f x
  Left  e <*> _       = Left e
  _       <*> Left  e = Left e

instance Monad (Either e) where
  Right x >>= f = f x
  Left  e >>= _ = Left e

instance Monoid e => MonadPlus (Either e) where
  mzero = Left mempty
  Left  _ `mplus` b = b
  Right x `mplus` _ = Right x


instance Functor [] where
  fmap = map

instance Pointed [] where
  pure x = [x]

instance Applicative [] where
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Monad [] where
  join = concat

instance MonadPlus [] where
  mzero = []
  mplus = (++)

instance Monoid [a] where
  mempty  = []
  mappend = (++)


newtype ZipList a = ZipList {getZipList :: [a]}

instance Functor ZipList where
  fmap f = ZipList . map f . getZipList

instance Pointed ZipList where
  -- pure x = Ziplist [x] doesn't satisfy the identity law: pure id <*> v = v
  -- because v might contain more than one element, and
  -- zipWith returns a list with length of the smaller of the two input sizes
  pure = ZipList . repeat

instance Applicative ZipList where
  ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs

-- ZipList is not a monad


instance Functor ((,) e) where
  fmap f (e, x) = (e, f x)

instance Monoid e => Pointed ((,) e) where
  pure x = (mempty, x)

instance Monoid e => Applicative ((,) e) where
  (e1, f) <*> (e2, x) = (e1 `mappend` e2, f x)

instance Monoid e => Monad ((,) e) where
  join (e1, (e2, x)) = (e1 `mappend` e2, x)


instance Functor ((->) e) where
  fmap = (.)

instance Pointed ((->) e) where
  pure = const

instance Applicative ((->) e) where
  (f <*> g) x = f x (g x)

instance Monad ((->) e) where
  join f x = f x x


newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
  mempty                      = Endo id
  (Endo f) `mappend` (Endo g) = Endo (f . g)



-- Functor utilities
void :: Functor f => f a -> f ()
void = fmap $ const ()


-- Applicative utilities

-- synonym for fmap
(<$>) :: Applicative f => (a -> b) -> f a -> f b
f <$> x = pure f <*> x

(<$) :: Applicative f => b -> f a -> f b
b <$ x = const b <$> x

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)

-- another synonym for fmap
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f x = f <$> x

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f x y z = f <$> x <*> y <*> z


-- Alternative utilities
asum :: Alternative f => [f a] -> f a
asum = foldr (<|>) empty


-- Monad utilities

-- synonym for fmap
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f x = x >>= (return . f)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb =
  ma >>= \a ->
  mb >>= \b ->
  return $ f a b

-- synonym for (<*>)
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma =
  mf >>= \f ->
  ma >>= \a ->
  return $ f a

sequence :: Monad m => [m a] -> m [a]
sequence = foldr (liftM2 (:)) (return [])

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n = sequence . replicate n

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n = sequence_ . replicate n

when :: Monad m => Bool -> m () -> m ()
when True  x = x
when False _ = return ()

unless :: Monad m => Bool -> m () -> m ()
unless = when . not

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(f >=> g) x = f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = flip (>=>)

forever :: Monad m => m a -> m b
forever x = x >> forever x

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) =
  p x          >>= \cond ->
  filterM p xs >>= \ys   ->
  return $ if cond
           then x:ys
           else ys


-- MonadPlus utilities

guard :: MonadPlus m => Bool -> m ()
guard True  = return ()
guard False = mzero

msum :: MonadPlus m => [m a] -> m a
msum = foldr mplus mzero

mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter p ma =
  ma >>= \a ->
  if p a
  then ma
  else mzero

-- TODO: alternative
