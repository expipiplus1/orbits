{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Physics.Orbit.QuickCheck
  ( CircularOrbit(..)
  , EllipticOrbit(..)
  , ParabolicOrbit(..)
  , HyperbolicOrbit(..)
  , NonInclinedStateVectors(..)
  , unitOrbit
  ) where

import Data.UnitsOfMeasure            (Quantity, u)
import Data.UnitsOfMeasure.QuickCheck (PositiveQuantity (..))
import Test.QuickCheck.Checkers       (EqProp (..), eq)
import Linear.QuickCheck              ()
import Linear.V3
import Physics.Orbit                  (Distance, InclinationSpecifier (..),
                                       Orbit (..), PeriapsisSpecifier (..),
                                       Unitless)
import Physics.Orbit.State            (StateVectors (..))
import System.Random                  (Random)
import Test.QuickCheck                (Arbitrary (..), choose, oneof, suchThat)

{-# ANN module "HLint: ignore Reduce duplication" #-}

newtype CircularOrbit a = CircularOrbit {getCircularOrbit :: Orbit a}
  deriving(Show, Eq)

newtype EllipticOrbit a = EllipticOrbit {getEllipticOrbit :: Orbit a}
  deriving(Show, Eq)

newtype ParabolicOrbit a = ParabolicOrbit {getParabolicOrbit :: Orbit a}
  deriving(Show, Eq)

newtype HyperbolicOrbit a = HyperbolicOrbit {getHyperbolicOrbit :: Orbit a}
  deriving(Show, Eq)

newtype NonInclinedStateVectors a = NonInclinedStateVectors {getStateVectors :: StateVectors a}
  deriving(Show, Eq)

instance (Num a, Ord a, Random a, Arbitrary a) => Arbitrary (Orbit a) where
  arbitrary = oneof
                [ getCircularOrbit <$> arbitrary
                , getEllipticOrbit <$> arbitrary
                , getParabolicOrbit <$> arbitrary
                , getHyperbolicOrbit <$> arbitrary
                ]
  shrink = shrinkOrbit

instance (Num a, Ord a, Arbitrary a) => Arbitrary (CircularOrbit a) where
  arbitrary =
    do
      let eccentricity = 0
      PositiveQuantity periapsis <- arbitrary
      inclinationSpecifier <- arbitrary
      let periapsisSpecifier = Circular
      PositiveQuantity primaryGravitationalParameter <- arbitrary
      pure . CircularOrbit $ Orbit { .. }
  shrink (CircularOrbit o) = CircularOrbit <$> shrinkOrbit o

instance (Num a, Ord a, Random a, Arbitrary a) => Arbitrary (EllipticOrbit a) where
  arbitrary =
    do
      eccentricity <- choose (0, 1) `suchThat` (/= 1)
      PositiveQuantity periapsis <- arbitrary
      inclinationSpecifier <- arbitrary
      periapsisSpecifier <- arbitrary
      PositiveQuantity primaryGravitationalParameter <- arbitrary
      pure . EllipticOrbit $ Orbit { .. }
  shrink (EllipticOrbit o) = EllipticOrbit <$> shrinkOrbit o

instance (Num a, Ord a, Arbitrary a) => Arbitrary (ParabolicOrbit a) where
  arbitrary =
    do
      let eccentricity = 1
      PositiveQuantity periapsis <- arbitrary
      inclinationSpecifier <- arbitrary
      periapsisSpecifier <- arbitrary
      PositiveQuantity primaryGravitationalParameter <- arbitrary
      pure . ParabolicOrbit $ Orbit { .. }
  shrink (ParabolicOrbit o) = ParabolicOrbit <$> shrinkOrbit o

instance (Num a, Ord a, Arbitrary a) => Arbitrary (HyperbolicOrbit a) where
  arbitrary =
    do
      eccentricity <- arbitrary `suchThat` (> 1)
      PositiveQuantity periapsis <- arbitrary
      inclinationSpecifier <- arbitrary
      periapsisSpecifier <- arbitrary
      PositiveQuantity primaryGravitationalParameter <- arbitrary
      pure . HyperbolicOrbit $ Orbit { .. }
  shrink (HyperbolicOrbit o) = HyperbolicOrbit <$> shrinkOrbit o

instance Arbitrary a => Arbitrary (InclinationSpecifier a) where
  arbitrary = oneof [pure NonInclined, Inclined <$> arbitrary <*> arbitrary]
  shrink Inclined { .. } = [NonInclined]
  shrink NonInclined     = []

-- | The instnace of Arbitrary for PeriapsisSpecifier doesn't generate Circular
instance (Eq a, Num a, Arbitrary a) => Arbitrary (PeriapsisSpecifier a) where
  arbitrary = Eccentric <$> arbitrary
  shrink (Eccentric x) = if x == [u|0 rad|] then [] else [Eccentric [u|0 rad|]]
  shrink Circular = []

instance Eq a => EqProp (Orbit a) where
  (=-=) = eq

instance Eq a => EqProp (CircularOrbit a) where
  (=-=) = eq

instance Eq a => EqProp (EllipticOrbit a) where
  (=-=) = eq

instance Eq a => EqProp (ParabolicOrbit a) where
  (=-=) = eq

instance Eq a => EqProp (HyperbolicOrbit a) where
  (=-=) = eq

instance (Eq a, Num a, Arbitrary a) => Arbitrary (StateVectors a) where
  arbitrary = do
    r <- arbitrary `suchThat` (/= pure [u|0m|])
    v <- arbitrary `suchThat` (/= pure [u|0m/s|])
    pure $ StateVectors r v

instance (Eq a, Num a, Arbitrary a) => Arbitrary (NonInclinedStateVectors a) where
  arbitrary = do
    StateVectors (V3 rx ry _) (V3 vx vy _) <- arbitrary
    pure $ NonInclinedStateVectors (StateVectors (V3 rx ry [u|0m|]) (V3 vx vy [u|0m/s|]))

instance Eq a => EqProp (StateVectors a) where
  (=-=) = eq

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


--------------------------------------------------------------------------------
-- Extras
--------------------------------------------------------------------------------

unitOrbit :: Num a => Orbit a
unitOrbit = Orbit{ eccentricity = 0
                 , periapsis    = [u|1m|]
                 , inclinationSpecifier = NonInclined
                 , periapsisSpecifier = Circular
                 , primaryGravitationalParameter = [u|1m^3s^-2|]
                 }
