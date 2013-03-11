This is a literate Haskell file, so if you want, play with it in GHCi!
It solves an exercise in group theory from Herstein's "Topics in Algebra":
Prove that given any two functions which permute three elements,
their composition is also a function that permutes three elements.
(That is, show the symmetric group S_3 satisfies the group axiom of closure.)

I got this problem for homework when I was studying abstract algebra, but
I didn't want to compute 36 function compositions by hand, so I wrote
this program to answer the question for me. It has some sentimental value for me
because it was the first nontrivial Haskell program I wrote.

I think it's a cute program, but I wasn't sure if anyone doing Hacker School
admissions knows Haskell, so I decided to spend an afternoon making it readable
using what I've learned recently about literate programming. Enjoy!


First, some library functions I'll use later:

> import Data.List (elemIndex)
> import Control.Monad (forever)

To start my own code I declare the list of permutations I'll be using later.

> permutations :: [Int -> Int]
> permutations = [f0, f1, f2, f3, f4, f5, f6]

Now I need to fill it. First, a dummy to fool Haskell's zero-based indexing,

> f0 = const 0

because the functions I called f1 to f6 are usually named phi_1 to phi_6, and
it will prove convenient for f_n to be at index n in my list of permutations.


The following functions describe the 3! = 6 possible permutations of [1, 2, 3].

> f1 = id
>
> f2 1 = 2
> f2 2 = 3
> f2 3 = 1
>
> f3 = f2 . f2
>
> f4 1 = 2
> f4 2 = 1
> f4 3 = 3
>
> f5 = f2 . f4
>
> f6 = f2 . f2 . f4


This is the domain of all of the functions in S_3.

> input :: [Int]
> input = [1, 2, 3]

The nth index of output contains the result of mapping f_n over [1, 2, 3].
I make use of this correspondence between indices below.

> output :: [[Int]]
> output = [map f input | f <- permutations]


A bit of theory before we come to the main function:
You cannot directly compare functions for equality, because it is undecidable
*in general*. However, you can simply brute force it in the finite case:
two functions are equal if and only if for each input they give the same output.
I use this strategy.

My homework was to hand in a chart, showing that I had exhaustively computed the
results of each possible function composition. I therefore wrote my main to run
interactively until interrupted, so I could fill in a few answers at a time.

main asks for two numbers, which it interprets as elements of S_3.
It composes them and applies the resulting function to the input [1, 2, 3].
The answer is the index where this result can already be found in "output".
This works because of the correspondence between indices of "output" and
"permutations", mentioned above.

> main :: IO ()
> main = forever $ do
>   putStrLn "Enter the numbers of the two permutations you want to compose."
>   ans1 <- getLine
>   ans2 <- getLine
>   let i1 = read ans1 :: Int
>       i2 = read ans2 :: Int
>       f = (permutations !! i1) . (permutations !! i2)
>       -- (.) is function composition; (!!) is list indexing
>       a = elemIndex (map f input) output
>   putStrLn $ show a ++ "\n"

I enjoyed and learned from writing this, and I hoped you liked reading it!

