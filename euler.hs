ex1 :: Integral a => a
ex1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]


ex2 :: Integral c => c
ex2 = sum . filter even . takeWhile (< 4000000) $ fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
