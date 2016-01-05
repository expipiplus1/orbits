{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Main
  ( main
  ) where

import Data.CReal (CReal)
import Data.CReal.QuickCheck ()
import Data.Maybe (fromJust)
import Data.UnitsOfMeasure.Extra (u, (*:), (/:), negate', square, cube, signum')
import Physics.Orbit
import Physics.Orbit.QuickCheck
import Physics.Radian (halfTurn)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, (===), (==>))
import Test.Tasty.TH (defaultMainGenerator)

{-# ANN module "HLint: ignore Reduce duplication" #-}

type Exact = CReal 64

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

test_meanAnomalyAtTime :: [TestTree]
test_meanAnomalyAtTime = [ testProperty "meanAnomaly signum"
                             (\o t -> signum' (meanAnomalyAtTime (o::Orbit Double) t) === signum' t)
                         , testProperty "meanAnomaly at t = 0"
                             (\o -> meanAnomalyAtTime (o::Orbit Double) [u|0s|] === [u|0rad|])
                         ]

main :: IO ()
main = $(defaultMainGenerator)

