{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Metrology.QuickCheck
  ( PositiveQuantity(..)
  ) where

import           Data.Metrology.Unsafe
import           System.Random                  ( Random )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , Positive(..)
                                                )
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                )

newtype PositiveQuantity a = PositiveQuantity { getPositiveQuantity :: a }
  deriving(Show)

deriving instance Arbitrary a => Arbitrary (Qu u l a)

deriving instance Random a => Random (Qu u l a)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (PositiveQuantity (Qu u l a)) where
  arbitrary = PositiveQuantity . Qu . getPositive <$> arbitrary
  shrink (PositiveQuantity (Qu x)) =
    PositiveQuantity . Qu . getPositive <$> shrink (Positive x)

instance (Eq a) => EqProp (Qu u l a) where
  (=-=) = eq

