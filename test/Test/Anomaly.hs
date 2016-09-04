{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Test.Anomaly
  ( test_conversions
  ) where

import Data.Coerce               (coerce)
import Data.CReal.QuickCheck     ()
import Data.Maybe                (fromJust)
import Data.UnitsOfMeasure.Extra (div', u, unQuantity, (/:))
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Checkers  (inverse)
import Test.Tasty                (TestTree, testGroup)
import Test.Tasty.QuickCheck     (testProperty, (===))

import Physics.Orbit
import Physics.Orbit.Anomaly
import Physics.Orbit.QuickCheck
import Physics.Radian           (halfTurn, turn)

import Test.Exact
import Test.SlowTest
import WrappedAngle  (WrappedAngle (..))

anomalyConversionTests :: (forall a. (RealFloat a, Show a, Arbitrary a, Converge [a])
                       => Orbit a -> Angle a -> Angle a)
                       -> String -> String -> [TestTree]
anomalyConversionTests convertAnomaly fromName toName =
  [ testProperty (toName ++ " when " ++ fromName ++ " = 0")
     (\(EllipticOrbit o) ->
       let to = convertAnomaly (o :: Orbit Double) [u|0rad|]
       in to === [u|0rad|])

  , testProperty (toName ++ " when " ++ fromName ++ " = π")
     (\(EllipticOrbit o) ->
       let to = convertAnomaly (o :: Orbit Double) halfTurn
       in to === halfTurn)

  , testProperty (toName ++ " when " ++ fromName ++ " = 2π")
     (\(EllipticOrbit o) ->
       let to = convertAnomaly (o :: Orbit Double) turn
       in to === turn)

  , testProperty "identity on circular orbits"
     (\(CircularOrbit o) from ->
       let to = convertAnomaly (o :: Orbit Exact) from
       in from === to)

  , testProperty "orbit number preservation"
     (\(EllipticOrbit o) from ->
       let to = convertAnomaly (o :: Orbit Double) from
       in from `div'` turn === (to `div'` turn :: Unitless Integer))
  ]

timeAnomalyConversionTests :: (forall a. (RealFloat a, Show a, Arbitrary a, Converge [a])
                                      => Orbit a -> Time a -> Angle a)
                           -> String -> [TestTree]
timeAnomalyConversionTests timeToAnomaly toName =
  [ testProperty (toName ++ " when time = 0")
     (\(EllipticOrbit o) ->
       let to = timeToAnomaly (o :: Orbit Double) [u|0s|]
       in to === [u|0rad|])

  , testProperty (toName ++ " when time = p/2")
     (\(EllipticOrbit o) ->
       let to = timeToAnomaly (o :: Orbit Exact) (p/:2)
           Just p = period o
       in to === halfTurn)

  , testProperty (toName ++ " when time = p")
     (\(EllipticOrbit o) ->
       let to = timeToAnomaly (o :: Orbit Exact) p
           Just p = period o
       in to === turn)

  , testProperty "identity on the unit orbit (modulo units!)"
     (\time ->
       let o = unitOrbit
           to = timeToAnomaly (o :: Orbit Exact) time
       in unQuantity time === unQuantity to)

  , testProperty "orbit number preservation"
     (\(EllipticOrbit o) time ->
       let to = timeToAnomaly (o :: Orbit Double) time
           Just p = period o
       in time `div'` p === (to `div'` turn :: Unitless Integer))
  ]

anomalyTimeConversionTests :: (forall a. (RealFloat a, Show a, Arbitrary a, Converge [a])
                                      => Orbit a -> Angle a -> Time a)
                           -> String -> [TestTree]
anomalyTimeConversionTests anomalyToTime fromName =
  [ testProperty ("time when " ++ fromName ++ " = 0")
     (\(EllipticOrbit o) ->
       let t = anomalyToTime (o :: Orbit Double) [u|0rad|]
       in t === [u|0s|])

  , testProperty ("time when " ++ fromName ++ " = π")
     (\(EllipticOrbit o) ->
       let t = anomalyToTime (o :: Orbit Double) halfTurn
           Just p = period o
       in t === p /: 2)

  , testProperty ("time when " ++ fromName ++ " = 2π")
     (\(EllipticOrbit o) ->
       let t = anomalyToTime (o :: Orbit Double) turn
           Just p = period o
       in t === p)

  , testProperty "identity on the unit orbit (modulo units!)"
     (\from ->
       let o = unitOrbit
           t = anomalyToTime (o :: Orbit Exact) from
       in unQuantity from === unQuantity t)

  , testProperty "orbit number preservation"
     (\(EllipticOrbit o) from ->
       let t = anomalyToTime (o :: Orbit Double) from
           Just p = period o
       in from `div'` turn === (t `div'` p :: Unitless Integer))
  ]

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
f .: g = \x y -> f (g x y)

test_conversions :: [TestTree]
test_conversions = [ conversionToTime
                   , conversionToMeanAnomaly
                   , conversionToEccentricAnomaly
                   , conversionToTrueAnomaly
                   , conversionInverses
                   ]
  where
    conversionToTime = testGroup "conversion to time"
      [ testGroup "from mean anomaly"
                  (anomalyTimeConversionTests timeAtMeanAnomaly "mean anomaly")
      , testGroup "from eccentric anomaly"
                  (anomalyTimeConversionTests (fromJust .: timeAtEccentricAnomaly)
                                              "eccentric anomaly")
      , testGroup "from true anomaly"
                  (anomalyTimeConversionTests (fromJust .: timeAtTrueAnomaly)
                                              "true anomaly")
      ]

    conversionToMeanAnomaly = let s = "mean anomaly" in testGroup ("conversion to " ++ s)
      [ testGroup "from time"
                  (timeAnomalyConversionTests meanAnomalyAtTime s)
      , testGroup "from eccentric anomaly"
                  (anomalyConversionTests (fromJust .: meanAnomalyAtEccentricAnomaly)
                                          "eccentric anomaly"
                                          s)
      , testGroup "from true anomaly"
                  (anomalyConversionTests (fromJust .: meanAnomalyAtTrueAnomaly)
                                          "true anomaly"
                                          s)
      ]

    conversionToEccentricAnomaly = let s = "eccentric anomaly" in testGroup ("conversion to " ++ s)
      [ testGroup "from time"
                  (timeAnomalyConversionTests (fromJust .: eccentricAnomalyAtTime) s)
      , testGroup "from mean anomaly"
                  (anomalyConversionTests (fromJust .: eccentricAnomalyAtMeanAnomaly)
                                          "mean anomaly"
                                          s)
      , testGroup "from true anomaly"
                  (anomalyConversionTests (fromJust .: eccentricAnomalyAtTrueAnomaly)
                                          "true anomaly"
                                          s)
      ]

    conversionToTrueAnomaly = let s = "true anomaly" in testGroup ("conversion to " ++ s)
      [ testGroup "from time"
                  (timeAnomalyConversionTests (fromJust .: trueAnomalyAtTime) s)
      , testGroup "from mean anomaly"
                  (anomalyConversionTests (fromJust .: trueAnomalyAtMeanAnomaly)
                                          "mean anomaly"
                                          s)
      , testGroup "from eccentric anomaly"
                  (anomalyConversionTests (fromJust .: trueAnomalyAtEccentricAnomaly)
                                          "eccentric anomaly"
                                          s)
      ]

    conversionInverses = testGroup "conversionInverses"
      [ testProperty "mean time inverse"
          (\o -> inverse (meanAnomalyAtTime (o :: Orbit Exact))
                         (timeAtMeanAnomaly o))

      , slowTest $ testProperty "mean eccentric inverse"
          (\(EllipticOrbit o) ->
            inverse (coerce (fromJust . meanAnomalyAtEccentricAnomaly (o :: Orbit Exact)) :: WrappedAngle Exact -> WrappedAngle Exact)
                    (coerce (fromJust . eccentricAnomalyAtMeanAnomaly o)))

      , slowTest $ testProperty "mean true inverse"
          (\(EllipticOrbit o) ->
            inverse (fromJust . meanAnomalyAtTrueAnomaly (o :: Orbit Exact))
                    (fromJust . trueAnomalyAtMeanAnomaly o))

      , slowTest $ testProperty "time true inverse"
          (\(EllipticOrbit o) ->
            inverse (fromJust . timeAtTrueAnomaly (o :: Orbit Exact))
                    (fromJust . trueAnomalyAtTime o))

      , slowTest $ testProperty "time eccentric inverse"
          (\(EllipticOrbit o) ->
            inverse (fromJust . timeAtEccentricAnomaly (o :: Orbit Exact))
                    (fromJust . eccentricAnomalyAtTime o))

      , slowTest $ testProperty "eccentric true inverse"
          (\(EllipticOrbit o) ->
            inverse (coerce (fromJust . eccentricAnomalyAtTrueAnomaly (o:: Orbit Exact)) :: WrappedAngle Exact -> WrappedAngle Exact)
                    (fromJust . coerce (trueAnomalyAtEccentricAnomaly o)))
      ]
