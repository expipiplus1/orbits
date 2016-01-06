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
  , apoapsis
  , meanMotion
  , period
  , semiMajorAxis
  , semiMinorAxis
  , semiLatusRectum
  , hyperbolicApproachAngle
  , hyperbolicDepartureAngle
    -- ** Conversions
  , meanAnomalyAtTime

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

import Data.UnitsOfMeasure.Extra
import Data.UnitsOfMeasure.Defs ()
import Data.UnitsOfMeasure.Show ()
import Linear.V3 (V3)
import Physics.Radian (turn)

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

-- | Calculate the distance between the bodies when they are at their most
-- distant. 'apoapsis' returns 'Nothing' when given a parabolic or hyperbolic
-- orbit.
apoapsis :: (Fractional a, Ord a) => Orbit a -> Maybe (Distance a)
apoapsis o =
  case classify o of
    Elliptic -> Just $ a *: (1 +: e)
    _        -> Nothing
  where
    Just a = semiMajorAxis o
    e = eccentricity o

-- | Calculate the mean motion, n, of an orbit
--
-- This is the rate of change of the mean anomaly with respect to time.
meanMotion :: (Floating a, Ord a) => Orbit a -> Quantity a [u|rad/s|]
meanMotion o =
  case classify o of
    Elliptic   -> convert $ sqrt' (μ /: cube a)
    Hyperbolic -> convert $ sqrt' (μ /: negate' (cube a))
    Parabolic  -> convert $ 2 *: sqrt' (μ /: cube l)
  where
    Just a = semiMajorAxis o
    μ = primaryGravitationalParameter o
    l = semiLatusRectum o

-- | Calculate the orbital period, p, of an elliptic orbit.
--
-- 'period' returns Nothing if given a parabolic or hyperbolic orbit.
period :: (Floating a, Ord a) => Orbit a -> Maybe (Time a)
period o =
  case classify o of
    Elliptic -> Just p
    _ -> Nothing
  where
    n = meanMotion o
    p = turn /: n

-- | Calculate the angle at which a body leaves the system when on an escape
-- trajectory relative to the argument of periapsis. This is the limit of the
-- true anomaly as time tends towards infinity minus the argument of periapsis.
-- The departure angle is in the closed range (π/2..π).
--
-- This is the negation of the approach angle.
--
-- 'hyperbolicDepartureAngle' returns Nothing when given an elliptic orbit and
-- π when given a parabolic orbit.
hyperbolicDepartureAngle :: (Floating a, Ord a) => Orbit a -> Maybe (Angle a)
hyperbolicDepartureAngle o =
  case classify o of
    Hyperbolic ->
      let e = eccentricity o
          θ = convert $ acos (-1 / e)
      in Just θ
    Parabolic -> Just (turn /: 2)
    _ -> Nothing

-- | Calculate the angle at which a body leaves the system when on a hyperbolic
-- trajectory relative to the argument of periapsis. This is the limit of the
-- true anomaly as time tends towards -infinity minus the argument of
-- periapsis. The approach angle is in the closed range (-π..π/2).
--
-- This is the negation of the departure angle.
--
-- 'hyperbolicApproachAngle' returns Nothing when given a non-hyperbolic orbit
-- and -π when given a parabolic orbit.
hyperbolicApproachAngle :: (Floating a, Ord a) => Orbit a -> Maybe (Angle a)
hyperbolicApproachAngle = fmap negate' . hyperbolicDepartureAngle

-- | Calculate the <https://en.wikipedia.org/wiki/Mean_anomaly mean anomaly>,
-- 'M', at the given time since periapse, t. T may be negative, indicating that
-- the orbiting body has yet to reach periapse.
--
-- The sign of the mean anomaly at time t is the same as the sign of t.
meanAnomalyAtTime :: (Floating a, Ord a) => Orbit a -> Time a -> Angle a
meanAnomalyAtTime o t = t *: n
  where n = meanMotion o
