{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.CReal.QuickCheck
  ( module Data.CReal
  ) where

import Data.CReal
import GHC.TypeLits
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrarySizedFractional, shrinkRealFracToInteger)

instance KnownNat n => Arbitrary (CReal n) where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkRealFracToInteger

