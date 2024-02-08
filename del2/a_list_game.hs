solve :: [Integer] -> [Integer]
solve [] = []
solve x = [solve2 (x !! 0) 2 0]

solve2 :: Integer -> Integer -> Integer -> Integer
solve2 a b ans =
    if a `mod` b == 0 then solve2 (a `div` b) b (ans + 1) 
    else if b * b < a then solve2 a (b + 1) ans
    else if a /= 1 then ans + 1
    else ans

readInput = (map read) . words
writeOutput = unlines . (map show)

main = interact (writeOutput . solve . readInput)