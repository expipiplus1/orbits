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
import Data.UnitsOfMeasure (u, (*:), negate')
import Physics.Orbit
import Physics.Orbit.QuickCheck
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, (===))
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

main :: IO ()
main = $(defaultMainGenerator)

