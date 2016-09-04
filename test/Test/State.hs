{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Test.State
  ( test_state
  ) where

import Data.Coerce               (coerce)
import Data.CReal.QuickCheck     ()
import Data.Maybe                (fromJust, isJust, isNothing)
import Data.UnitsOfMeasure.Extra (One, Quantity, div', u, unQuantity, (+:),
                                  (/:))
import Linear.Metric
import Linear.QuickCheck         ()
import Linear.V3
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Checkers  (inverse)
import Test.Tasty                (TestTree, testGroup)
import Test.Tasty.QuickCheck     (testProperty, (===), (==>))

import Physics.Orbit            hiding (isValid)
import Physics.Orbit.QuickCheck
import Physics.Orbit.State
import Physics.Radian           (halfTurn, turn)

import Test.Exact
import Test.SlowTest
import WrappedAngle  (WrappedAngle (..))

test_sanity :: [TestTree]
test_sanity = [ testProperty "StateVectors isValid"
                  (\rv -> isValid (rv :: StateVectors Double))
              ]

test_state :: [TestTree]
test_state = test_sanity ++ [angularTests, quaternionTests]
  where
    angularTests = testGroup "angular momentum"
      [ testProperty "ascending node vector is valid"
          (\rv -> ascendingNodeVector rv /= Just (V3 0 0 (0 :: Quantity Double One)))

      , testProperty "ascending node exists when inclined"
          (\rv -> isInclined rv ==> isJust (ascendingNodeVector (rv :: StateVectors Double)))

      , testProperty "no ascending node when non-inclined"
          (\(NonInclinedStateVectors rv) -> isNothing (ascendingNodeVector (rv :: StateVectors Double)))

      , testProperty "no inclination"
          (\(NonInclinedStateVectors rv) -> inclinationFromStateVectors (rv :: StateVectors Double) == NonInclined)

      -- , testProperty "inclinationFromStateVectors"

      -- , testProperty "eccentricity vector magnitude"

      -- , testProperty "semi-major axis length"

      -- , testProperty "l = h^2 / μ"

      -- , testProperty "distance at ν = 0"

      -- , testProperty "distance at ν = π"

      , testProperty "E = K + V"
          (\μ rv -> specificMechanicalEnergy μ (rv :: StateVectors Exact) === specificKineticEnergy rv +: gravitationalPotential μ rv)

      ]

    quaternionTests = testGroup "orbital plane rotation"
      [ testProperty "inverse worldToPlane planeToWorld"
          (\o -> inverse (worldToPlane o) (planeToWorld (o :: Orbit Exact)))
      , testProperty "norm (orbitalPlaneQuaternion o) = 1"
          (\o -> norm (orbitalPlaneQuaternion o) == (1 :: Exact))
      ]

