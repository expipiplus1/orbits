{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Test.State
  ( test_state
  ) where

import Data.Coerce               (coerce)
import Data.CReal.QuickCheck     ()
import Data.Maybe                (fromJust, isJust, isNothing)
import Data.UnitsOfMeasure.Extra (One, Quantity, div', norm', square, u, quadrance',
                                  unQuantity, (*:), (+:), (/:))
import Data.UnitsOfMeasure.Defs
import Data.UnitsOfMeasure.QuickCheck (PositiveQuantity (..))
import Control.Arrow((&&&))
import Data.Coerce(coerce)

import Linear.Metric
import Linear.QuickCheck         ()
import Linear.V3
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Checkers  (inverse, idempotent)
import Test.Tasty                (TestTree, testGroup)
import Test.Tasty.QuickCheck     (testProperty, (===), (==>), (.||.))

import Physics.Orbit            hiding (isValid)
import Physics.Orbit.QuickCheck
import Physics.Orbit.State
import Physics.Radian           (halfTurn, turn)

import Linear.Conjugate

import Test.Exact
import Test.SlowTest
import WrappedAngle  (WrappedAngle (..))

import Debug.Trace

test_sanity :: [TestTree]
test_sanity = [ testProperty "StateVectors isValid"
                  (\rv -> isValid (rv :: StateVectors Double))
              ]

test_hyperbolicState :: [TestTree]
test_hyperbolicState = [
                         {- TODO: reenable once trueAnomalyAtTime is finished
                         testProperty "v < v∞"
                           (\(HyperbolicOrbit o) t ->
                               let ν = trueAnomalyAtTime o t
                               in fromJust (hyperbolicExcessVelocity (o :: Orbit Double)) < speedAtTrueAnomaly o ν)


                       , testProperty "v^2 = vesc^2 + v∞^2"
                          (\(HyperbolicOrbit o) t ->
                              let ν = trueAnomalyAtTime o t
                                  r = distanceAtTrueAnomaly o (ν :: Angle Exact)
                                  μ = primaryGravitationalParameter o
                              in square (speedAtTrueAnomaly o ν) === square (escapeVelocity μ r) +: square (fromJust (hyperbolicExcessVelocity o))))
                              -}
                       ]

test_state :: [TestTree]
test_state = test_sanity
          ++ test_hyperbolicState
          ++ [angularTests, quaternionTests]
  where
    angularTests = testGroup "angular momentum"
      [ testProperty "ascending node vector is valid"
          (\rv -> ascendingNodeVector rv /= Just (V3 0 0 (0 :: Quantity Double One)))

      , testProperty "ascending node exists when inclined"
          (\rv -> isInclined rv ==> isJust (ascendingNodeVector (rv :: StateVectors Double)))

      , testProperty "no ascending node when non-inclined"
          (\(NonInclinedStateVectors rv) -> isNothing (ascendingNodeVector (rv :: StateVectors Double)))

      , testProperty "no inclination"
          (\(NonInclinedStateVectors rv) -> 
            let i = inclinationFromStateVectors (rv :: StateVectors Double) 
            in i === EquatorialPrograde .||. i === EquatorialRetrograde)

      , testProperty "inclination"
          (\rv -> 
            let i = inclinationFromStateVectors (rv :: StateVectors Double) 
            in isInclined rv ==> (case i of 
                                    Inclined{} -> True
                                    _          -> False)
          )

      , testProperty "speed = norm velocity"
          (\o ν -> square (speedAtTrueAnomaly o ν)
               === quadrance' (velocityAtTrueAnomaly o (ν :: Angle Exact)))

      , testProperty "speed = norm velocityInPlaneAtTrueAnomaly"
          (\o ν -> square (speedAtTrueAnomaly o ν)
               === quadrance' (velocityInPlaneAtTrueAnomaly o (ν :: Angle Exact)))

      , testProperty "distance = norm position"
          (\o ν -> square (distanceAtTrueAnomaly o ν)
               === quadrance' (positionAtTrueAnomaly o (ν :: Angle Exact)))

      , testProperty "distance = norm positionInPlaneAtTrueAnomaly"
          (\o ν -> square (distanceAtTrueAnomaly o ν)
               === quadrance' (positionInPlaneAtTrueAnomaly o (ν :: Angle Exact)))
    
      , testProperty "is normalized"
          (\rv (PositiveQuantity μ) -> 
            let o = orbitFromStateVectors μ rv
            in o === normalizeOrbit (o :: Orbit Double)
          )

      , testProperty "toState fromState inverse"
          (inverse ((\(o, ν) -> 
                     ( stateVectorsFromTrueAnomaly o ν
                     , PositiveQuantity $ primaryGravitationalParameter (o :: Orbit Exact)
                     ) 
                   ))
                   ((\(rv, PositiveQuantity μ) -> 
                     ( orbitFromStateVectors μ rv
                     , trueAnomalyFromStateVectors μ rv
                     ) 
                   ))
          )

      -- , testProperty "inclinationFromStateVectors"

      -- , testProperty "eccentricity vector magnitude"

      -- , testProperty "semi-major axis length"

--       , testProperty "p = 2πab / h"
--           (\(EllipticOrbit o) ν ->
--             let rv = stateVectorsFromTrueAnomaly o ν
--                 h = norm' (specificAngularMomentum rv)
--                 Just a = semiMajorAxis o
--                 b = semiMinorAxis o
--                 Just p = period (o :: Orbit Exact)
--             in p === (2 *: pi *: a *: b) /: h
--           )

--       , testProperty "l = h^2 / μ"
--           (\μ rv ->
--             let o = orbitFromStateVectors μ rv
--                 h = norm' (specificAngularMomentum rv)
--                 l = semiLatusRectum (o :: Orbit Exact)
--             in l === square h /: μ
--           )

      -- , testProperty "distance at ν = 0"

      -- , testProperty "distance at ν = π"

      -- , testProperty "distance on circular orbit"

      -- , testProperty "distance <= apoapsis"

      -- , testProperty "distance >= periapsis"

      , testProperty "E = K + V"
          (\μ rv -> specificMechanicalEnergy μ (rv :: StateVectors Exact) === specificKineticEnergy rv +: gravitationalPotential μ rv)

      ]

    quaternionTests = testGroup "orbital plane rotation"
      [ testProperty "inverse worldToPlane planeToWorld"
          (\o -> inverse (worldToPlane o) (planeToWorld (o :: Orbit Exact)))
      , testProperty "norm (orbitalPlaneQuaternion o) = 1"
          (\o -> norm (orbitalPlaneQuaternion o) == (1 :: Exact))
      ]

a :: (Ord a, RealFloat a, Conjugate a) => (Orbit a, Angle a) -> (StateVectors a, PositiveQuantity (Quantity a [u| m^3 s^-2 |]))
a = (\(o, ν) -> ( stateVectorsFromTrueAnomaly o ν
                               , PositiveQuantity $ primaryGravitationalParameter o 
                               )
                   )

b :: (Show a, Ord a, RealFloat a) => (StateVectors a, PositiveQuantity (Quantity a [u| m^3 s^-2 |]))  -> (Orbit a, Angle a)
b =           (\(rv, PositiveQuantity μ) -> ( orbitFromStateVectors μ rv
                           , trueAnomalyFromStateVectors μ rv
                           )
                   )

type A = Exact

r :: (Orbit A, Angle A)
r =         (Orbit {eccentricity = [u| 0.99999 |], periapsis = [u| 1.0000000000 m |], inclinationSpecifier = EquatorialPrograde, periapsisSpecifier = Eccentric {argumentOfPeriapsis = [u| 0.0000000000 rad |]}, primaryGravitationalParameter = [u| 1.0000000000 m^3 / s^2 |]},[u| 0.0000000000 rad |])

i = a r

v :: (StateVectors A, PositiveQuantity (Quantity A [u| m^3 * (1 / s^2)|]))
v = (StateVectors {position = V3 [u| -0.5081253157 m |] [u| 6.4167164995 m |] [u| -1.6048873600 m |], velocity = V3 [u| -6.0604177122 m / s |] [u| 0.3691382715 m / s |] [u| 0.9954491090 m / s |]},PositiveQuantity {getPositiveQuantity = [u| 1.0000000000 m^3 / s^2 |]})

o :: (Orbit A, Angle A)
o =         (Orbit {eccentricity = [u| 2.0000000000 |], periapsis = [u| 1.0000000000 m |], inclinationSpecifier = EquatorialPrograde, periapsisSpecifier = Eccentric {argumentOfPeriapsis = [u| 0.0000000000 rad |]}, primaryGravitationalParameter = [u| 1.0000000000 m^3 / s^2 |]},[u| 3.0000000000 rad |])


o2 :: (Orbit A, Angle A)
o2 = (Orbit {eccentricity = [u| 0.0000000000 |], periapsis = [u| 1.0000000000 m |], inclinationSpecifier = Inclined {longitudeOfAscendingNode = [u| -3.1075312924 rad |], nonEquatorialInclination = [u| -2.0877463710 rad |]}, periapsisSpecifier = Circular, primaryGravitationalParameter = [u| 1.0000000000 m^3 / s^2 |]},[u| 0.0000000000 rad |])

