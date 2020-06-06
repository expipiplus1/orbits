{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Physics.Orbit.QuickCheck
  ( CircularOrbit(..)
  , EllipticOrbit(..)
  , ParabolicOrbit(..)
  , HyperbolicOrbit(..)
  , CanonicalOrbit(..)
  , pattern CircularOrbitF
  , pattern EllipticOrbitF
  , pattern ParabolicOrbitF
  , pattern HyperbolicOrbitF
  , unitOrbit
  , overAllClasses
  ) where

import           Data.Constants.Mechanics.Extra
import           Data.Metrology
import           Data.Metrology.Extra           ( mod' )
import           Data.Metrology.QuickCheck
import           Data.Metrology.Unsafe
import           Data.Units.SI.Parser
import           Linear.V3
import           Physics.Orbit                  ( Distance
                                                , InclinationSpecifier(..)
                                                , Orbit(..)
                                                , PeriapsisSpecifier(..)
                                                , Unitless
                                                )
import           Physics.Orbit.StateVectors
import           System.Random                  ( Random )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , Testable
                                                , choose
                                                , oneof
                                                , suchThat
                                                )
import           Test.Tasty                     ( TestTree )
import           Test.Tasty.QuickCheck          ( testProperty )

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

newtype CircularOrbit a = CircularOrbit {getCircularOrbit :: Orbit a}
  deriving(Show, Eq)

newtype EllipticOrbit a = EllipticOrbit {getEllipticOrbit :: Orbit a}
  deriving(Show, Eq)

newtype ParabolicOrbit a = ParabolicOrbit {getParabolicOrbit :: Orbit a}
  deriving(Show, Eq)

newtype HyperbolicOrbit a = HyperbolicOrbit {getHyperbolicOrbit :: Orbit a}
  deriving(Show, Eq)

-- | An orbit where all angles are in [0..2π) or [0..π)
--
-- Also not a weird orbit like circular or non inclined
newtype CanonicalOrbit a = CanonicalOrbit {getCanonicalOrbit :: Orbit a}
  deriving(Show, Eq)

pattern CircularOrbitF :: Orbit Float -> CircularOrbit Float
pattern CircularOrbitF o = CircularOrbit o

pattern EllipticOrbitF :: Orbit Float -> EllipticOrbit Float
pattern EllipticOrbitF o = EllipticOrbit o

pattern ParabolicOrbitF :: Orbit Float -> ParabolicOrbit Float
pattern ParabolicOrbitF o = ParabolicOrbit o

pattern HyperbolicOrbitF :: Orbit Float -> HyperbolicOrbit Float
pattern HyperbolicOrbitF o = HyperbolicOrbit o

-- | Use aerobreaking to shrink an orbit without expending fuel
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

instance (Num a, Ord a, Random a, Arbitrary a) => Arbitrary (ParabolicOrbit a) where
  arbitrary =
    do
      let eccentricity = 1
      PositiveQuantity periapsis <- arbitrary
      inclinationSpecifier <- arbitrary
      periapsisSpecifier <- arbitrary
      PositiveQuantity primaryGravitationalParameter <- arbitrary
      pure . ParabolicOrbit $ Orbit { .. }
  shrink (ParabolicOrbit o) = ParabolicOrbit <$> shrinkOrbit o

instance (Num a, Ord a, Random a, Arbitrary a) => Arbitrary (HyperbolicOrbit a) where
  arbitrary =
    do
      eccentricity <- arbitrary `suchThat` (> 1)
      PositiveQuantity periapsis <- arbitrary
      inclinationSpecifier <- arbitrary
      periapsisSpecifier <- arbitrary
      PositiveQuantity primaryGravitationalParameter <- arbitrary
      pure . HyperbolicOrbit $ Orbit { .. }
  shrink (HyperbolicOrbit o) = HyperbolicOrbit <$> shrinkOrbit o

instance (Floating a, Real a, Random a, Arbitrary a) => Arbitrary (CanonicalOrbit a) where
  arbitrary = do
    PositiveQuantity eccentricity <- arbitrary
    PositiveQuantity periapsis    <- arbitrary
    PositiveQuantity _Ω           <- arbitrary
    PositiveQuantity i            <- arbitrary
    let inclinationSpecifier =
          Inclined (_Ω `mod'` turn) (i `mod'` (halfTurn |/| 2))
    ω <- arbitrary
    let periapsisSpecifier = Eccentric (ω `mod'` turn)
    PositiveQuantity primaryGravitationalParameter <- arbitrary
    pure . CanonicalOrbit $ Orbit { .. }
  -- shrink (CanonicalOrbit o) = CanonicalOrbit <$> shrinkOrbit o

instance Arbitrary a => Arbitrary (InclinationSpecifier a) where
  arbitrary = oneof [pure NonInclined, Inclined <$> arbitrary <*> arbitrary]
  shrink Inclined {..} = [NonInclined]
  shrink NonInclined   = []

-- | The instance of Arbitrary for PeriapsisSpecifier doesn't generate Circular
instance (Eq a, Num a, Arbitrary a) => Arbitrary (PeriapsisSpecifier a) where
  arbitrary = Eccentric <$> arbitrary
  shrink (Eccentric x) = [Eccentric zero | x /= zero]
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
shrinkPeriapsis a | a == Qu 1 = []
                  | otherwise = [Qu 1]

shrinkPrimaryGravitationalParameter
  :: (Num a, Eq a)
  => MkQu_ULN [si|m^3 s^-2|] 'DefaultLCSU a
  -> [MkQu_ULN [si|m^3 s^-2|] 'DefaultLCSU a]
shrinkPrimaryGravitationalParameter μ | μ == Qu 1 = []
                                      | otherwise = [Qu 1]


--------------------------------------------------------------------------------
-- Extras
--------------------------------------------------------------------------------

unitOrbit :: Fractional a => Orbit a
unitOrbit = Orbit{ eccentricity = 0
                 , periapsis    = 1 % [si|m|]
                 , inclinationSpecifier = NonInclined
                 , periapsisSpecifier = Circular
                 , primaryGravitationalParameter = 1 % [si|m^3 s^-2|]
                 }


----------------------------------------------------------------
-- Constructing test trees
----------------------------------------------------------------

overAllClasses
  :: (Random a, Arbitrary a, Num a, Ord a, Show a, Testable t)
  => (Orbit a -> t)
  -> [TestTree]
overAllClasses t =
  [ testProperty "circular"   (\(CircularOrbit o) -> t o)
  , testProperty "elliptic"   (\(EllipticOrbit o) -> t o)
  , testProperty "parabolic"  (\(ParabolicOrbit o) -> t o)
  , testProperty "hyperbolic" (\(HyperbolicOrbit o) -> t o)
  ]


----------------------------------------------------------------
-- StateVectors
----------------------------------------------------------------

instance (Num a, Eq a, Arbitrary a) => Arbitrary (StateVectors a) where
  arbitrary =
    do
        r <- V3 <$> arbitrary <*> arbitrary <*> arbitrary
        v <- V3 <$> arbitrary <*> arbitrary <*> arbitrary
        pure $ StateVectors r v
      `suchThat` (\(StateVectors r v) ->
                   r /= V3 zero zero zero && v /= V3 zero zero zero
                 )
