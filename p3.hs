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

largestPrimeFactor :: Int -> Int
largestPrimeFactor !n
  | n <= 1 = error "largestPrimeFactor n where n <= 1"
  | otherwise = largestPrimeFactor' n (2 : [3, 5..])
  where
    largestPrimeFactor' n pseudoprimeCandidates@(c:cs)
      | c * c >= n = n
      | m == 0 = largestPrimeFactor' d pseudoprimeCandidates
      | otherwise = largestPrimeFactor' n cs
      where
        !(!d, !m) = divMod n c

{-
rhoPoly :: Int -> Int -> Int
rhoPoly !x !n = mod (x * x - 1) n

pollardStep :: Int -> Int -> Int -> Int -> Int -> Int
pollardStep !i !k !n !x !y
  | d/= 1 && d/= n = d
  | i == k = pollardStep (i + 1) (2 * k) n x1 x1
  | otherwise = pollardStep (i + 1) k n x1 y
  where d = gcd n $ abs $ y - x
        x1 = rhoPoly x n

pollardRho :: Int -> Int
pollardRho !n = pollardStep 1 2 n 2 2
-}

main :: IO ()
main = do
  TIO.putStrLn (problem <> "\n")
  !(!ts, !a) <- Chronos.stopwatch solve
  TIO.putStrLn ("The answer is: " <> showt a)
  TIO.putStrLn ""
  TIO.putStrLn ("It took " <> Chronos.encodeTimespan (Chronos.SubsecondPrecisionFixed 6) ts <> " seconds to compute.")

solve :: IO Int
solve = do
  let !x = 600851475143
  pure $ largestPrimeFactor x

problem :: Text
problem = mconcat
  [ "The prime factors of 13195 are 5, 7, 13, and 29."
  , "\n"
  , "What is the largest prime factor of the number 600851475143?"
  ]