{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.QuickCheck
  () where

import Linear
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = sequenceA (pure arbitrary)

instance Eq a => EqProp (V3 a) where
  (=-=) = eq

