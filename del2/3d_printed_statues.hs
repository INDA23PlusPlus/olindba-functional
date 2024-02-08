solve :: [Integer] -> [Integer]
solve [] = []
solve x = [solve2 (x !! 0) 1 1]

solve2 :: Integer -> Integer -> Integer -> Integer
solve2 a b ans =
    if b < a then solve2 a (b * 2) (ans + 1) 
    else ans

readInput = (map read) . words
writeOutput = unlines . (map show)

main = interact (writeOutput . solve . readInput)