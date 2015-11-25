{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Physics.Orbit.QuickCheck
  ( CircularOrbit(..)
  , EllipticOrbit(..)
  , ParabolicOrbit(..)
  , HyperbolicOrbit(..)
  ) where

import Data.UnitsOfMeasure (u, Quantity)
import Data.UnitsOfMeasure.QuickCheck (PositiveQuantity(..))
import Physics.Orbit (Orbit(..), InclinationSpecifier(..), PeriapsisSpecifier(..), Unitless, Distance)
import Test.QuickCheck (Arbitrary(..), oneof)

newtype CircularOrbit a = CircularOrbit (Orbit a)
  deriving(Show, Eq)

newtype EllipticOrbit a = EllipticOrbit (Orbit a)
  deriving(Show, Eq)

newtype ParabolicOrbit a = ParabolicOrbit (Orbit a)
  deriving(Show, Eq)

newtype HyperbolicOrbit a = HyperbolicOrbit (Orbit a)
  deriving(Show, Eq)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (CircularOrbit a) where
  arbitrary =
    do let eccentricity = 0
       PositiveQuantity periapsis <- arbitrary
       inclinationSpecifier <- arbitrary
       let periapsisSpecifier = Circular
       PositiveQuantity primaryGravitationalParameter <- arbitrary
       pure . CircularOrbit $ Orbit{..}
  shrink (CircularOrbit o) = CircularOrbit <$> shrinkOrbit o

instance Arbitrary a => Arbitrary (InclinationSpecifier a) where
  arbitrary = oneof [pure NonInclined, Inclined <$> arbitrary <*> arbitrary]
  shrink Inclined{..} = [NonInclined]
  shrink NonInclined = []

-- | The instnace of Arbitrary for PeriapsisSpecifier doesn't generate Circular
instance (Eq a, Num a, Arbitrary a) => Arbitrary (PeriapsisSpecifier a) where
  arbitrary = Eccentric <$> arbitrary
  shrink (Eccentric x) = if x == [u|0 rad|] then [] else [Eccentric [u|0 rad|]]
  shrink Circular = []

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

-- | Note, this doesn't just lower the altitude, ho ho
shrinkOrbit :: (Arbitrary a, Num a, Ord a) => Orbit a -> [Orbit a]
shrinkOrbit o = [o{eccentricity = e} | e <- shrinkEccentricity (eccentricity o)] ++
                [o{periapsis = q} | q <- shrinkPeriapsis (periapsis o)] ++
                [o{inclinationSpecifier = i} | i <- shrink (inclinationSpecifier o)] ++
                [o{periapsisSpecifier = ω} | ω <- shrink (periapsisSpecifier o)] ++
                [o{primaryGravitationalParameter = μ} | μ <-
                    shrinkPrimaryGravitationalParameter (primaryGravitationalParameter o)]

-- The semantics for shrinking lots of these values isn't to necessrily to
-- get a smaller value, but a more simple integral value could make
-- debugging easier. Try and skrink to the integers 0, 1, and 2
shrinkEccentricity :: (Num a, Ord a) => Unitless a -> [Unitless a]
shrinkEccentricity e | e == 0 || e == 1 || e == 2 = []
                     | e < 1 = [0]
                     | e > 1 = [2]
                     | otherwise = error "shrinkEccentricity"

shrinkPeriapsis :: (Num a, Eq a) => Distance a -> [Distance a]
shrinkPeriapsis a | a == [u|1m|] = []
                  | otherwise = [[u|1m|]]

shrinkPrimaryGravitationalParameter :: (Num a, Eq a) => Quantity a [u|m^3 s^-2|] -> [Quantity a [u|m^3 s^-2|]]
shrinkPrimaryGravitationalParameter μ | μ == [u|1 m^3 s^-2|] = []
                                      | otherwise = [[u|1 m^3 s^-2|]]
