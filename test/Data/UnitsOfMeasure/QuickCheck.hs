{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UnitsOfMeasure.QuickCheck
  ( PositiveQuantity(..)
  ) where

import Test.QuickCheck (Arbitrary(..), Positive(..))
import Data.UnitsOfMeasure.Internal (Quantity(..))

newtype PositiveQuantity a = PositiveQuantity { getPositiveQuantity :: a }

deriving instance Arbitrary a => Arbitrary (Quantity a u)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (PositiveQuantity (Quantity a u)) where
  arbitrary = PositiveQuantity . MkQuantity . getPositive <$> arbitrary
  shrink (PositiveQuantity x) = PositiveQuantity <$> shrink x

