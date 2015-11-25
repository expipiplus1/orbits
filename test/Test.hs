{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import Physics.Orbit
import Physics.Orbit.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH (defaultMainGenerator)

prop_validCircularOrbit :: CircularOrbit Double -> Bool
prop_validCircularOrbit (CircularOrbit o) = isValid o

prop_validEllipticOrbit :: EllipticOrbit Double -> Bool
prop_validEllipticOrbit (EllipticOrbit o) = isValid o

prop_validParabolicOrbit :: ParabolicOrbit Double -> Bool
prop_validParabolicOrbit (ParabolicOrbit o) = isValid o

prop_validHyperbolicOrbit :: HyperbolicOrbit Double -> Bool
prop_validHyperbolicOrbit (HyperbolicOrbit o) = isValid o

main :: IO ()
main = $(defaultMainGenerator)

