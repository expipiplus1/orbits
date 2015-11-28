{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UnitsOfMeasure.QuickCheck
  ( PositiveQuantity(..)
  ) where

import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Random (Random)
import Test.QuickCheck (Arbitrary(..), Positive(..))

newtype PositiveQuantity a = PositiveQuantity { getPositiveQuantity :: a }

deriving instance Arbitrary a => Arbitrary (Quantity a u)

deriving instance Random a => Random (Quantity a u)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (PositiveQuantity (Quantity a u)) where
  arbitrary = PositiveQuantity . MkQuantity . getPositive <$> arbitrary
  shrink (PositiveQuantity x) = PositiveQuantity <$> shrink x

