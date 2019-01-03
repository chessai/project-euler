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
import Data.Semigroup (Max(..))
import TextShow (showt)

main :: IO ()
main = do
  TIO.putStrLn (problem <> "\n")
  !(!ts, !a) <- Chronos.stopwatch solve
  TIO.putStrLn ("The answer is: " <> showt a)
  TIO.putStrLn ""
  TIO.putStrLn ("It took " <> Chronos.encodeTimespan (Chronos.SubsecondPrecisionFixed 6) ts <> " seconds to compute.")

largestPalindrome :: [Int] -> Int
largestPalindrome xs = getMax (foldlMap' (Max . filterPal) xs)
  where
    filterPal !x = if isPalindrome x then x else 0

threeDigitsProducts :: [Int] -> [Int] -> [Int]
threeDigitsProducts x y = (*) <$> x <*> y

scaleLargestPalindrome :: [[Int]] -> Int
scaleLargestPalindrome [] = 0
scaleLargestPalindrome (xs : xxs) =
  let prods = threeDigitsProducts xs xs
  in case largestPalindrome prods of { 0 -> scaleLargestPalindrome xxs; x -> x; }

isPalindrome :: Int -> Bool
isPalindrome !n
  | n < 10 = True
  | n `mod` 10 == 0 = False
  | otherwise = go (n `div` 10) (n `mod` 10)
  where go !num !reverse = if num >= 1
          then go (num `div` 10) ((reverse * 10) + (num `mod` 10))
          else if n == reverse then True else False

solve :: IO Int
solve = pure $ scaleLargestPalindrome
  [ [900..999]
  , [800..899]
  , [700..799]
  , [600..699]
  , [500..599]
  , [400..499]
  , [300..399]
  , [200..299]
  , [100..199]
  ]

problem :: Text
problem = mconcat
  [ "A palindromic number reads the same both ways. The largest palindrome made from the product of two"
  , " 2-digit numbers is 9009=91 x 99."
  , "\n"
  , "\n"
  , "Find the largest palindrome made from the product of two 3-digit numbers."
  ]