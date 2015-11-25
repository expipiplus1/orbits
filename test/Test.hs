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

main :: IO ()
main = $(defaultMainGenerator)

