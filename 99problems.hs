-- following "99 Haskell Problems", but with safety
-- http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems

-- problem 1
myLast :: [a] -> Maybe a
myLast []     = Nothing
myLast [x]    = Just x
myLast (_:xs) = myLast xs

-- problem 2
myPenultimate :: [a] -> Maybe a
myPenultimate []     = Nothing
myPenultimate [_]    = Nothing
myPenultimate [x, _] = Just x
myPenultimate (_:xs) = myPenultimate xs

-- problem 3
elementAt :: [a] -> Int -> Maybe a
elementAt []     _ = Nothing
elementAt (x:_)  1 = Just x
elementAt (_:xs) n = elementAt xs (n-1)

-- problem 4
myLength :: [a] -> Int
myLength = foldr ((+) . (const 1)) 0

-- problem 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- problem 6
palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == myReverse xs

-- problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys) = if x == y
                    then compress (y:ys)
                    else x : compress (y:ys)

-- problem 9
