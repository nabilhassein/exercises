import Data.List          (elemIndex)
import System.Environment (getArgs)

-- I discussed GHC's non-optimization of this function with some friends at
-- Hacker School. I was surprised that a fast compiler for a pure language
-- didn't memoize it, but a better implementation is easy, and found below.
naiveFib :: Int -> Integer
naiveFib    0    = 0
naiveFib    1    = 1
naiveFib    n    = naiveFib (n-2) + naiveFib (n-1)


-- This function runs in linear time. I basically copied and pasted it from:
-- http://www.haskell.org/haskellwiki/Memoization#Memoization_with_recursion
memoizedFib :: Int -> Integer
memoizedFib  = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib k = memoizedFib (k-2) + memoizedFib (k-1)
-- ... however, if you write the first line of the function as
-- > memoized_fib n = map fib [0..] !! n
-- then it will be as slow as naive_fib. Why does GHC treat these differently?


-- This implementation is particularly elegant, and also runs in linear time.
-- It have seen it many times in many places, but I do not know its true origin.
-- Despite superficial differences, it is fundamentally similar to memoized_fib
-- in that it indexes a lazily constructed infinite list.
cuteFib :: Int -> Integer
cuteFib    = (fibs !!)
  where
    fibs :: [Integer]
    fibs  = 0 : 1 : zipWith (+) fibs (tail fibs)


-- This computation of the closed-form solution is imperfect, as shown below:
-- https://en.wikipedia.org/wiki/Fibonacci_number#Closed-form_expression
closedFib :: Int -> Integer
closedFib    n    = round $ phi^n / sqrt 5
  where
    phi :: Double
    phi  = (1 + sqrt 5) / 2


-- This function demonstrates the above closed form solution to be accurate only
-- through the 75th fibonacci number, due to floating point error.
-- (If you change phi's type annotation to Haskell's single-precision Float, 
-- it is accurate for just 30.)
-- It is not immediately obvious to me how to improve it. Using a representation
-- of floating point numbers with greater than 64 bits is possible, but I do not
-- know how fast or easy it is to use and/or implement. Perhaps see
-- http://www.haskell.org/haskellwiki/Exact_real_arithmetic
-- I am no expert, but I think integer addition (e.g. cuteFib) is likely faster.
divergence :: Maybe Int -- divergence = Just 76
divergence  = let iterativeFibs  = map cuteFib   [0..]
                  closedFormFibs = map closedFib [0..]
              in  firstUnequalIndex iterativeFibs closedFormFibs
  where
    firstUnequalIndex :: Eq a => [a] -> [a] -> Maybe Int
    firstUnequalIndex            xs   = elemIndex False . zipWith (==) xs


main :: IO ()
main = getArgs >>= print . cuteFib . read . head
