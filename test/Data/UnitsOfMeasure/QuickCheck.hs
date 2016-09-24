{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UnitsOfMeasure.QuickCheck
  ( PositiveQuantity(..)
  ) where

import Data.UnitsOfMeasure.Internal (Quantity (..), unQuantity)
import System.Random                (Random)
import Test.QuickCheck              (Arbitrary (..), Positive (..))
import Test.QuickCheck.Checkers     (EqProp (..), eq)

newtype PositiveQuantity a = PositiveQuantity { getPositiveQuantity :: a }
  deriving(Show, Eq)

deriving instance Arbitrary a => Arbitrary (Quantity a u)

deriving instance Random a => Random (Quantity a u)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (PositiveQuantity (Quantity a u)) where
  arbitrary = PositiveQuantity . MkQuantity . getPositive <$> arbitrary
  shrink (PositiveQuantity x) =
    PositiveQuantity . MkQuantity . getPositive <$> shrink (Positive . unQuantity $ x)

instance (Eq a) => EqProp (Quantity a u) where
  (=-=) = eq

instance (Eq a) => EqProp (PositiveQuantity a) where
  (=-=) = eq
