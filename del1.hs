 import Data.List (sort)
 
 -- Fibonacci
 fib 0 = 0
 fib 1 = 1
 fib (n) = fib(n-1) + fib(n-2)

 --Reverse list
 rev [] = []
 rev (x:xs) = rev(xs) ++ [x]

 --Median length of strings
 median :: [String] -> Float
 median list | 
    len == 0 = 0.0 | 
    odd len = fromIntegral (sorted !! mid) | 
    even len = fromIntegral (sorted !! mid + sorted !! (mid - 1)) / 2
        where
            len = length list
            mid = len `div` 2
            sorted = sort (lengths list)

 lengths :: [String] ->  [Int]
 lengths [] = []
 lengths (x:xs) = [length x] ++ lengths(xs)