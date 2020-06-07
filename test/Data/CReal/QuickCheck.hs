{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.CReal.QuickCheck
  ( module Data.CReal
  ) where

import           Data.CReal
import           GHC.TypeLits
import           Linear.Conjugate
import           Linear.Epsilon
import           Test.QuickCheck.Arbitrary      ( Arbitrary(..)
                                                , arbitrarySizedFractional
                                                , shrinkRealFrac
                                                )

instance KnownNat n => Arbitrary (CReal n) where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkRealFrac

instance TrivialConjugate (CReal n) where
instance Conjugate (CReal n) where

instance Epsilon (CReal n) where
  nearZero = const False
