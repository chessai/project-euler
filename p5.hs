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

lcm' :: [Int] -> Int
lcm' = getLCM . foldlMap' LCM

newtype LCM a = LCM { getLCM :: a }

instance Integral a => Semigroup (LCM a) where
  (LCM x) <> (LCM y) = LCM (lcm x y)

instance Integral a => Monoid (LCM a) where
  mempty = LCM 1

solve :: IO Int
solve = pure $ lcm' [1..20]

problem :: Text
problem = mconcat
  [ "2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder."
  , "\n\n"
  , "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
  ]