{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import Physics.Orbit
import Physics.Orbit.QuickCheck
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, (===))
import Test.Tasty.TH (defaultMainGenerator)

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


main :: IO ()
main = $(defaultMainGenerator)

