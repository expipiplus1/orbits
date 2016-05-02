{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Main
  ( main
  ) where

import           Data.Coerce               (coerce)
import           Data.CReal                (CReal)
import           Data.CReal.QuickCheck     ()
import           Data.Maybe                (fromJust)
import           Data.UnitsOfMeasure.Extra (cube, div', negate', square, u,
                                            unQuantity, (*:), (/:))
import           Physics.Orbit
import           Physics.Orbit.QuickCheck
import           Physics.Radian            (halfTurn, turn)
import           Test.QuickCheck.Arbitrary (Arbitrary)
import           Test.QuickCheck.Checkers  (inverse)
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.QuickCheck     (testProperty, (===), (==>))
import           Test.Tasty.TH             (defaultMainGenerator)
import           WrappedAngle              (WrappedAngle (..))

{-# ANN module "HLint: ignore Reduce duplication" #-}

type Exact = CReal 32

test_sanity :: [TestTree]
test_sanity = [ testProperty "circular isValid"
                  (\(CircularOrbit o) -> isValid (o :: Orbit Double))
              , testProperty "elliptic isValid"
                  (\(EllipticOrbit o) -> isValid (o :: Orbit Double))
              , testProperty "parabolic isValid"
                  (\(ParabolicOrbit o) -> isValid (o :: Orbit Double))
              , testProperty "hyperbolic isValid"
                  (\(HyperbolicOrbit o) -> isValid (o :: Orbit Double))
              ]

test_classify :: [TestTree]
test_classify = [ testProperty "circular"
                    (\(CircularOrbit o) ->
                       classify (o :: Orbit Double) === Elliptic)
                , testProperty "elliptic"
                    (\(EllipticOrbit o) ->
                       classify (o :: Orbit Double) === Elliptic)
                , testProperty "parabolic"
                    (\(ParabolicOrbit o) ->
                       classify (o :: Orbit Double) === Parabolic)
                , testProperty "hyperbolic"
                    (\(HyperbolicOrbit o) ->
                       classify (o :: Orbit Double) === Hyperbolic)
                ]

test_semiMajorAxis :: [TestTree]
test_semiMajorAxis = [ testProperty "circular"
                         (\(CircularOrbit o) ->
                            fromJust (semiMajorAxis (o :: Orbit Double)) === periapsis o)
                     , testProperty "elliptic"
                         (\(EllipticOrbit o) ->
                            fromJust (semiMajorAxis (o :: Orbit Double)) > [u|0m|])
                     , testProperty "parabolic"
                         (\(ParabolicOrbit o) ->
                            semiMajorAxis (o :: Orbit Double) === Nothing)
                     , testProperty "hyperbolic"
                         (\(HyperbolicOrbit o) ->
                            fromJust (semiMajorAxis (o :: Orbit Double)) < [u|0m|])
                     ]

test_semiMinorAxis :: [TestTree]
test_semiMinorAxis = [ testGroup "range"
                         [ testProperty "elliptic: b > 0"
                             (\(EllipticOrbit o) ->
                                semiMinorAxis (o :: Orbit Double) > [u|0m|])
                         , testProperty "parabolic: b = 0"
                             (\(ParabolicOrbit o) ->
                                semiMinorAxis (o :: Orbit Double) === [u|0m|])
                         , testProperty "hyperbolic: b < 0"
                             (\(HyperbolicOrbit o) ->
                                semiMinorAxis (o :: Orbit Double) < [u|0m|])
                         ]
                     , testProperty "semiMinorAxis circular = q"
                         (\(CircularOrbit o) ->
                            semiMinorAxis (o :: Orbit Double) === periapsis o)
                     , testGroup "b^2 = a * l"
                         [ testProperty "elliptic"
                             (\(EllipticOrbit o) -> let a = fromJust (semiMajorAxis (o :: Orbit Exact))
                                                        b = semiMinorAxis o
                                                        l = semiLatusRectum o
                                                    in b *: b === a *: l)
                         , testProperty "hyperbolic"
                             (\(HyperbolicOrbit o) -> let a = fromJust (semiMajorAxis (o :: Orbit Exact))
                                                          b = semiMinorAxis o
                                                          l = semiLatusRectum o
                                                      in b *: b === negate' (a *: l))
                         ]
                     ]

test_apoapsis :: [TestTree]
test_apoapsis = [ testProperty "ap > q"
                    (\(EllipticOrbit o) ->
                       eccentricity (o :: Orbit Double) /= 0
                       ==> fromJust (apoapsis o) > periapsis o)
                , testProperty "circular: ap = q"
                    (\(CircularOrbit o) ->
                       fromJust (apoapsis (o :: Orbit Double)) === periapsis o)
                , testProperty "parabolic: no ap"
                    (\(ParabolicOrbit o) ->
                       apoapsis (o :: Orbit Double) === Nothing)
                , testProperty "hyperbolic: no ap"
                    (\(HyperbolicOrbit o) ->
                       apoapsis (o :: Orbit Double) === Nothing)
                ]

test_meanMotion :: [TestTree]
test_meanMotion = [ testProperty "n > 0"
                      (\o -> meanMotion (o :: Orbit Double) > [u|0rad/s|])
                  ]

test_period :: [TestTree]
test_period = [ testProperty "p > 0"
                  (\(EllipticOrbit o) ->
                     fromJust (period (o :: Orbit Double)) > [u|0s|])
                , testProperty "4 π a^3 / p^2 = μ"
                    (\(EllipticOrbit o) ->
                      let Just p = period (o :: Orbit Exact)
                          Just a = semiMajorAxis o
                          μ = primaryGravitationalParameter o
                      in (4 * square pi) *: cube a /: square p === μ)
                , testProperty "parabolic: no p"
                    (\(ParabolicOrbit o) ->
                       period (o :: Orbit Double) === Nothing)
                , testProperty "hyperbolic: no p"
                    (\(HyperbolicOrbit o) ->
                       period (o :: Orbit Double) === Nothing)
              ]

-- TODO: Put converge test here
test_hyperbolicAngles :: [TestTree]
test_hyperbolicAngles = [ testProperty "parabolic approach"
                            (\(ParabolicOrbit o) ->
                               fromJust
                                 (hyperbolicApproachAngle (o :: Orbit Double))
                                 === negate' halfTurn)
                        , testProperty "parabolic departure"
                            (\(ParabolicOrbit o) ->
                               fromJust
                                 (hyperbolicDepartureAngle (o :: Orbit Double))
                                 === halfTurn)
                        , testProperty "hyperbolic symmetry"
                            (\(HyperbolicOrbit o) ->
                               fromJust (hyperbolicDepartureAngle (o :: Orbit Double))
                               === negate' (fromJust (hyperbolicApproachAngle o)))
                        , testProperty "elliptic: no approach"
                            (\(EllipticOrbit o) ->
                               hyperbolicApproachAngle (o :: Orbit Double) === Nothing)
                        , testProperty "elliptic: no departure"
                            (\(EllipticOrbit o) ->
                               hyperbolicDepartureAngle (o :: Orbit Double) === Nothing)
                        ]

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
f .: g = (\x y -> f (g x y))

test_conversionToTime :: [TestTree]
test_conversionToTime =
  [ testGroup "from mean anomaly"
              (anomalyTimeConversionTests timeAtMeanAnomaly "mean anomaly")
  , testGroup "from eccentric anomaly"
              (anomalyTimeConversionTests (fromJust .: timeAtEccentricAnomaly)
                                          "eccentric anomaly")
  , testGroup "from true anomaly"
              (anomalyTimeConversionTests (fromJust .: timeAtTrueAnomaly)
                                          "true anomaly")
  ]

test_conversionToMeanAnomaly :: [TestTree]
test_conversionToMeanAnomaly = let s = "mean anomaly" in
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

test_conversionToEccentricAnomaly :: [TestTree]
test_conversionToEccentricAnomaly = let s = "eccentric anomaly" in
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

test_conversionToTrueAnomaly :: [TestTree]
test_conversionToTrueAnomaly = let s = "true anomaly" in
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

test_conversionInverses :: [TestTree]
test_conversionInverses =
  [ testProperty "mean time inverse"
      (\o -> inverse (meanAnomalyAtTime (o :: Orbit Exact))
                     (timeAtMeanAnomaly o))

  , testProperty "mean eccentric inverse"
      (\(EllipticOrbit o) ->
        inverse (coerce (fromJust . meanAnomalyAtEccentricAnomaly (o :: Orbit Exact)) :: WrappedAngle Exact -> WrappedAngle Exact)
                (coerce (fromJust . eccentricAnomalyAtMeanAnomaly o)))

  , testProperty "mean true inverse"
      (\(EllipticOrbit o) ->
        inverse (fromJust . meanAnomalyAtTrueAnomaly (o :: Orbit Exact))
                (fromJust . trueAnomalyAtMeanAnomaly o))

  , testProperty "time true inverse"
      (\(EllipticOrbit o) ->
        inverse (fromJust . timeAtTrueAnomaly (o :: Orbit Exact))
                (fromJust . trueAnomalyAtTime o))

  , testProperty "time eccentric inverse"
      (\(EllipticOrbit o) ->
        inverse (fromJust . timeAtEccentricAnomaly (o :: Orbit Exact))
                (fromJust . eccentricAnomalyAtTime o))

  , testProperty "eccentric true inverse"
      (\(EllipticOrbit o) ->
        inverse (coerce (fromJust . (eccentricAnomalyAtTrueAnomaly (o:: Orbit Exact))) :: WrappedAngle Exact -> WrappedAngle Exact)
                (fromJust . (coerce (trueAnomalyAtEccentricAnomaly o))))
  ]

main :: IO ()
main = $(defaultMainGenerator)

