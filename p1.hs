{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -O2 -fforce-recomp #-}

module Main (main) where

import Constrictor (foldlMap')
import Data.List.Extra (nubOrd)
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Chronos
import Data.Monoid (Sum(..))
import TextShow (showt)

main :: IO ()
main = do
  TIO.putStrLn (problem <> "\n")
  !(!ts, !a) <- Chronos.stopwatch solve
  TIO.putStrLn ("The answer is: " <> showt a)
  TIO.putStrLn ""
  TIO.putStrLn ("It took " <> Chronos.encodeTimespan (Chronos.SubsecondPrecisionFixed 6) ts <> " seconds to compute.")

solve :: IO Int
solve = pure $ solveProblem sumOfMultiples [3, 5] (1000 - 1)

problem :: Text
problem = mconcat
  [ "If we list all the natural numbers below 10 that are multiples of 3 or 5, "
  , "we get 3, 5, 6, and 9. The sum of these multiples is 23. "
  , "\n"
  , "Find the sum of all the multiples of 3 or 5 below 1000."
  ]

solveProblem :: (Int -> Int -> Int) -> [Int] -> Int -> Int
solveProblem f ls !bound = g ls - g duplicates
  where
    g :: [Int] -> Int 
    g ls'      = getSum (foldlMap' (Sum . f bound) ls')
    duplicates :: [Int]
    duplicates = nubOrd [x*y | x <- ls, y <- ls, x /= y]

sumOfMultiples :: Int -> Int -> Int
sumOfMultiples !x !d = d * (x `div` d) * (1 + x `div` d) `div` 2