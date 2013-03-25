-- an answer to the "exercise for the reader" in http://www.haskellforall.com/2012/01/haskell-for-mainstream-programmers_28.html

module Lens where

import Prelude hiding (id, (.))
import Control.Category

--               getter,    setter
type Lens a b = (a -> b , b -> a -> a)

getL :: Lens a b -> a -> b
getL = fst

setL :: Lens a b -> b -> a -> a
setL = snd

-- Apply a function to the field
-- Tekmo considers modL more fundamental than setL
modL :: Lens a b -> (b -> b) -> a -> a
modL l f a = setL l (f (getL l a)) a

(^.) :: a -> Lens a b -> b
a ^. p     = getL p a
-- or (^.) = flip getL

(^=) :: Lens a b -> b -> a -> a
(p ^= b) a = setL p b a
 -- or (^=) = setL