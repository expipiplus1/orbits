{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Test.Radian
  ( test_canonicalizeAngle
  ) where

import Data.UnitsOfMeasure.Extra ( negate')
import Data.UnitsOfMeasure.QuickCheck  hiding (classify)
import Test.QuickCheck.Extra
import Test.QuickCheck.Checkers  ( idempotent)
import Test.Tasty                (TestTree)
import Test.Tasty.QuickCheck     (testProperty)
import Physics.Orbit            (Angle)
import Physics.Radian           

test_canonicalizeAngle :: [TestTree]
test_canonicalizeAngle =
  [ testProperty "idempotent"
      (idempotent (canonicalizeAngle :: Angle Double -> Angle Double))

  , testProperty ">= -π"
      (\θ -> canonicalizeAngle θ >=! negate' (halfTurn :: Angle Double))

  , testProperty "< π"
      (\θ -> canonicalizeAngle θ <! (halfTurn :: Angle Double))
  ]

