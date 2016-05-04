module Test.QuickCheck.Extra
  ( (<=!)
  , (>=!)
  ) where

import Test.QuickCheck (Property, counterexample)

infix 4 <=!
(<=!) :: (Ord a, Show a) => a -> a -> Property
x <=! y = counterexample (show x ++ " ≰ " ++ show y) (x <= y)

infix 4 >=!
(>=!) :: (Ord a, Show a) => a -> a -> Property
x >=! y = counterexample (show x ++ " ≱ " ++ show y) (x >= y)

