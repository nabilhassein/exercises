-- basically copied and pasted from:
-- http://www.haskell.org/haskellwiki/Memoization#Memoization_with_recursion

memoized_fib :: Int -> Integer
memoized_fib    n    = map fib [0 ..] !! n
   where fib    0    = 0
         fib    1    = 1
         fib    k    = memoized_fib (k-2) + memoized_fib (k-1)

main :: IO ()
main = print $ memoized_fib 50
