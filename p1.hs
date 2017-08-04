import Data.List.Extra (nubOrd)

problem :: String
problem = "If we list all the natural numbers below 10 that are multiples of 3 or 5, " ++
          "we get 3, 5, 6, and 9. The sum of these multiples is 23. " ++
          "Find the sum of all the multiples of 3 or 5 below 1000"

solve :: String
solve = show $ solveProblem sumOfMultiples [3, 5] (1000 - 1)

solveProblem :: (Int -> Int -> Int) -> [Int] -> Int -> Int
solveProblem f ls bound = g ls - g duplicates
  where
    g ls'      = sum $ map (f bound) ls'
    duplicates = nubOrd [x*y | x <- ls, y <- ls, x /= y]

sumOfMultiples :: Int -> Int -> Int
sumOfMultiples x d = d * (x `div` d) * (1 + x `div` d) `div` 2
