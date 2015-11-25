-- Extensions for uom-plugin
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- Others
{-# LANGUAGE RecordWildCards #-}

-- | Types and functions for dealing with Kepler orbits.
module Physics.Orbit
  ( -- * The Orbit data type and dependencies
    Orbit(..)
  , InclinationSpecifier(..)
  , PeriapsisSpecifier(..)
  , Classification(..)

    -- * Functions for dealing with orbits
    -- ** Utilities
  , isValid
  , classify
    -- ** Orbital elements
  , semiMajorAxis
  , semiMinorAxis
  , semiLatusRectum

    -- * Unit synonyms
  , Time
  , Distance
  , Speed
  , Mass
  , Angle
  , Unitless
  , Position
  , Velocity
  ) where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Defs ()
import Data.UnitsOfMeasure.Show ()
import Linear.V3 (V3)
import Physics.Radian ()

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A measure in seconds.
type Time a     = Quantity a [u| s |]
-- | A measure in meters.
type Distance a = Quantity a [u| m |]
-- | A measure in meters per second.
type Speed a    = Quantity a [u| m s^-1 |]
-- | A measure in kilograms.
type Mass a     = Quantity a [u| kg |]
-- | A measure in radians.
type Angle a    = Quantity a [u| rad |]
-- | A unitless measure.
type Unitless a = Quantity a One
-- | A position in space.
type Position a = V3 (Distance a)
-- | A speed in space.
type Velocity a = V3 (Speed a)

-- | Data type defining an orbit parameterized by the type used to
-- represent values
data Orbit a = Orbit { -- | The orbit's eccentricity, e.
                       --
                       -- 'eccentricity' must be non-negative.
                       --
                       -- An eccentricity of 0 describes a circular orbit.
                       --
                       -- An eccentricity of less than 1 describes an elliptic
                       -- orbit.
                       --
                       -- An eccentricity equal to 1 describes a parabolic orbit.
                       --
                       -- An eccentricity greater than 1 describes a hyperbolic
                       -- orbit.
                       eccentricity :: !(Unitless a)
                       -- | The orbit's periapsis, q.
                       --
                       -- 'periapsis' must be positive.
                       --
                       -- The periapsis is the distance between the bodies at
                       -- their closest approach.
                     , periapsis :: !(Distance a)
                       -- | The 'inclinationSpecifier' describes the angle
                       -- between the obtital plane and the reference plane.
                     , inclinationSpecifier :: !(InclinationSpecifier a)
                       -- | 'periapsisSpecifier' is 'Circular' iff
                       -- 'eccentricity' is 0
                       --
                       -- The periapsis specifier describes any rotation of
                       -- the orbit relative to the reference direction in the
                       -- orbital plane.
                     , periapsisSpecifier :: !(PeriapsisSpecifier a)
                       -- | The gravitational parameter of the system's
                       -- primary, μ.
                       --
                       -- μ is equal to the mass of the primary times
                       -- <https://en.wikipedia.org/wiki/Gravitational_constant
                       -- G>.
                       --
                       -- 'primaryGravitationalParameter' must be positive.
                     , primaryGravitationalParameter :: !(Quantity a [u| m^3 s^-2 |])
                     }
  deriving (Show, Eq)

-- | Along with 'PeriapsisSpecifier' the 'InclinationSpecifier' describes
-- orbital elements extra to its geometry.
data InclinationSpecifier a = -- | The orbit does not lie exactly in the
                              -- reference plane
                              Inclined { -- | The longitude of the ascending
                                         -- node, Ω.
                                         --
                                         -- The angle between the reference
                                         -- direction and the point where the
                                         -- orbiting body crosses the reference
                                         -- plane in the positive z direction.
                                         longitudeOfAscendingNode :: !(Angle a)
                                         -- | The orbit's inclination, i.
                                         --
                                         -- The angle between the reference
                                         -- plane and the orbital plane
                                       , inclination :: !(Angle a)
                                       }
                              -- | The orbit lies in the reference plane
                            | NonInclined
  deriving (Show, Eq)

-- | Along with 'InclinationSpecifier' the 'PeriapsisSpecifier' describes
-- orbital elements extra to its geometry.
data PeriapsisSpecifier a = -- | The orbit is not circular
                            Eccentric { -- | The argument of periapsis, ω.
                                        --
                                        -- The 'argumentOfPeriapsis' is the
                                        -- angle of the periapsis relative to
                                        -- the reference direction in the
                                        -- orbital plane.
                                        argumentOfPeriapsis :: !(Angle a) }
                            -- | The orbit has an eccentricity of 0 so the
                            -- 'argumentOfPeriapsis' is indeterminate.
                          | Circular
  deriving (Show, Eq)

-- | What for the orbit's geometry takes. This is dependant only on the
-- 'eccentricity', e >= 0, of the orbit.
data Classification = -- | 0 <= e < 1
                      --
                      -- This includes circular orbits.
                      Elliptic
                      -- | e == 1
                    | Parabolic
                      -- | e > 1
                    | Hyperbolic
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Return true is the orbit is valid and false if it is invalid. The behavior
-- of all the other functions in this module is undefined when given an invalid
-- orbit.
isValid :: (Ord a, Num a) => Orbit a -> Bool
isValid o = e >= 0 &&
            ((e == 0) `iff` (periapsisSpecifier o == Circular)) &&
            q > [u|0 m|] &&
            μ > [u|0 m^3 s^-2|]
  where
    iff = (==) :: Bool -> Bool -> Bool
    e = eccentricity o
    q = periapsis o
    μ = primaryGravitationalParameter o

-- | 'classify' is a funciton which returns the orbit's class.
classify :: (Num a, Ord a) => Orbit a -> Classification
classify o
  | e < 1 = Elliptic
  | e == 1 = Parabolic
  | e > 1 = Hyperbolic
  | otherwise = error "classify"
  where
    e = eccentricity o

-- | Calculate the semi-major axis, a, of the 'Orbit'. Returns 'Nothing' when
-- given a parabolic orbit for which there is no semi-major axis. Note that the
-- semi-major axis of a hyperbolic orbit is negative.
semiMajorAxis :: (Fractional a, Ord a) => Orbit a -> Maybe (Distance a)
semiMajorAxis o =
  case classify o of
    Parabolic -> Nothing
    _         -> Just $ q /: (1 -: e)
  where
    q = periapsis o
    e = eccentricity o

-- | Calculate the semi-minor axis, b, of the 'Orbit'. Like 'semiMajorAxis'
-- @\'semiMinorAxis\' o@ is negative when @o@ is a hyperbolic orbit. In the
-- case of a parabolic orbit 'semiMinorAxis' returns 0m.
semiMinorAxis :: (Floating a, Ord a) => Orbit a -> Distance a
semiMinorAxis o =
  case classify o of
    Elliptic   -> a *: sqrt' (1 -: e ^ (2::Int))
    Parabolic  -> [u|0m|]
    Hyperbolic -> a *: sqrt' (e ^ (2::Int) -: 1)
  where
    e = eccentricity o
    Just a = semiMajorAxis o

-- | Calculate the semiLatusRectum, l, of the 'Orbit'
semiLatusRectum :: (Num a) => Orbit a -> Distance a
semiLatusRectum orbit = e *: q +: q
  where q = periapsis orbit
        e = eccentricity orbit
