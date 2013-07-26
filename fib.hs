-- I discussed GHC's non-optimization of this function with some friends at
-- Hacker School. I was surprised that a fast compiler for a pure language
-- didn't memoize it, but a better implementation is easy, and found below.
naive_fib :: Int -> Integer
naive_fib    0    = 0
naive_fib    1    = 1
naive_fib    n    = naive_fib (n-2) + naive_fib (n-1)

-- This function runs in linear time. I basically copied and pasted it from:
-- http://www.haskell.org/haskellwiki/Memoization#Memoization_with_recursion
memoized_fib :: Int -> Integer
memoized_fib    n    = map fib [0 ..] !! n
   where fib    0    = 0
         fib    1    = 1
         fib    k    = memoized_fib (k-2) + memoized_fib (k-1)

-- This implementation is particularly elegant, and also runs in linear time.
-- It have seen it many times in many places, but I do not know its true origin.
-- Despite superficial differences, it is fundamentally similar to memoized_fib
-- in that it indexes a lazily constructed infinite list.
cute_fib :: Int -> Integer
cute_fib    n    = fibs !! n
  where fibs :: [Integer]
        fibs  = 0 : 1 : zipWith (+) fibs (tail fibs)

-- This computation of the closed-form solution is imperfect:
-- https://en.wikipedia.org/wiki/Fibonacci_number#Closed-form_expression
closed_fib :: Int -> Integer
closed_fib    n    = round $ (phi^n - psi^n) / sqrt 5
  where phi, psi :: Double
        phi = (1 + (sqrt 5)) / 2
        psi = 1 - phi

-- This function demonstrates that above closed form solution is only accurate
-- through the 75th fibonacci number, presumably due to floating point error.
-- I wonder how to improve it? Arbitrary-precision arithmetic is probably
-- possible, but I do not how fast or easy it is to use/implement. Perhaps see
-- http://www.haskell.org/haskellwiki/Exact_real_arithmetic
divergence :: Int
divergence = let iterative_fibs   = map cute_fib   [0..]
                 closed_form_fibs = map closed_fib [0..]
             in  firstUnequal iterative_fibs closed_form_fibs
  where
    firstUnequal = go 0
    go :: Int -> [Integer] -> [Integer] -> Int
    go    n      (x:xs)       (y:ys)     = if x /= y then n else go (n+1) xs ys

main :: IO ()
main = print divergence
