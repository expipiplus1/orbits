module Test.QuickCheck.Extra
  ( (<=!)
  , (>=!)
  , (<!)
  , (>!)
  , module Test.QuickCheck
  ) where

import Test.QuickCheck

infix 4 <=!
(<=!) :: (Ord a, Show a) => a -> a -> Property
x <=! y = counterexample (show x ++ " ≰ " ++ show y) (x <= y)

infix 4 >=!
(>=!) :: (Ord a, Show a) => a -> a -> Property
x >=! y = counterexample (show x ++ " ≱ " ++ show y) (x >= y)

infix 4 <!
(<!) :: (Ord a, Show a) => a -> a -> Property
x <! y = counterexample (show x ++ " ≮ " ++ show y) (x < y)

infix 4 >!
(>!) :: (Ord a, Show a) => a -> a -> Property
x >! y = counterexample (show x ++ " ≯ " ++ show y) (x > y)

