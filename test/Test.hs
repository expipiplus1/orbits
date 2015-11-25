{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import Physics.Orbit
import Physics.Orbit.QuickCheck
import Test.Tasty.QuickCheck (testProperty, Property, (===))
import Test.Tasty.TH (defaultMainGenerator)

prop_validCircularOrbit :: CircularOrbit Double -> Bool
prop_validCircularOrbit (CircularOrbit o) = isValid o

prop_validEllipticOrbit :: EllipticOrbit Double -> Bool
prop_validEllipticOrbit (EllipticOrbit o) = isValid o

prop_validParabolicOrbit :: ParabolicOrbit Double -> Bool
prop_validParabolicOrbit (ParabolicOrbit o) = isValid o

prop_validHyperbolicOrbit :: HyperbolicOrbit Double -> Bool
prop_validHyperbolicOrbit (HyperbolicOrbit o) = isValid o

prop_classifyCircularOrbit :: CircularOrbit Double -> Property
prop_classifyCircularOrbit (CircularOrbit o) = classify o === Elliptic

prop_classifyEllipticOrbit :: EllipticOrbit Double -> Property
prop_classifyEllipticOrbit (EllipticOrbit o) = classify o === Elliptic

prop_classifyParabolicOrbit :: ParabolicOrbit Double -> Property
prop_classifyParabolicOrbit (ParabolicOrbit o) = classify o === Parabolic

prop_classifyHyperbolicOrbit :: HyperbolicOrbit Double -> Property
prop_classifyHyperbolicOrbit (HyperbolicOrbit o) = classify o === Hyperbolic


main :: IO ()
main = $(defaultMainGenerator)

