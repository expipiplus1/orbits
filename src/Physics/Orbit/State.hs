-- Extensions for uom-plugin
{-# LANGUAGE DataKinds           #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- | Types and functions for dealing with orbital state
module Physics.Orbit.State
  ( -- * Orbital state
    StateVectors(..)

    -- ** Utilities
  , isValid
  , distance
  , speed
  , specificAngularMomentum
  , isInclined
  , ascendingNodeVector
  , eccentricityVector

    -- ** Specific energies
  , specificKineticEnergy
  , gravitationalPotential
  , specificMechanicalEnergy

    -- ** Extracting orbital parameters from state vectors
  , inclinationFromStateVectors
  , periapsisFromStateVectors
  , argumentOfPeriapsisFromStateVectors
  , trueAnomalyFromStateVectors

    -- ** Extracting state vectors from orbital elements
    -- *** At the True anomaly
  , distanceAtTrueAnomaly
  , positionAtTrueAnomaly

    -- ** Rotations between reference frames
  , planeToWorld
  , worldToPlane
  , orbitalPlaneQuaternion

    -- ** Conversions between all elements
  , orbitFromStateVectors
  ,
  ) where

import Data.UnitsOfMeasure.Defs     ()
import Data.UnitsOfMeasure.Extra
import Data.UnitsOfMeasure.Internal (Quantity (..))
import Lens.Micro                   ((^.))
import Linear.Conjugate
import Linear.Metric                hiding (distance)
import Linear.Quaternion.Extra      hiding (Angle)
import Linear.V3
import Physics.Orbit                hiding (isValid)
import Physics.Radian

-- | A position and velocity of an orbiting body.
data StateVectors a = StateVectors
  { position :: Position a
  , velocity :: Velocity a
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Is this set of state vectors valid
isValid :: (Eq a, Num a) => StateVectors a -> Bool
isValid (StateVectors r v) = r /= pure [u|0m|] && v /= pure [u|0m/s|]

distance :: Floating a => StateVectors a -> Distance a
distance (StateVectors r _) = norm' r

speed :: Floating a => StateVectors a -> Speed a
speed (StateVectors _ v) = norm' v

speedSq :: Floating a => StateVectors a -> Quantity a [u|m^2 s^-2|]
speedSq (StateVectors _ v) = quadrance' v

-- | Compute the specific angular momentum of an orbit from its state vectors.
specificAngularMomentum :: Num a
                        => StateVectors a
                        -> V3 (Quantity a [u|m^2/s|])
specificAngularMomentum (StateVectors r v) = h
  where h = r `cross'` v

isInclined :: (Eq a, Num a) => StateVectors a -> Bool
isInclined (StateVectors r v) = r^._z /= [u|0m|] || v^._z /= [u|0m/s|]

-- | Compute the vector pointing towards the ascening node of an orbit from its
-- state vectors. If the orbit is not inclined then this function returns
-- nothing.
ascendingNodeVector :: (Eq a, Num a)
                    => StateVectors a
                    -> Maybe (V3 (Quantity a One))
ascendingNodeVector rv
  | isInclined rv = Just (mk <$> n)
  | otherwise     = Nothing
  where n = k `cross` (unQuantity <$> h)
        h = specificAngularMomentum rv
        k = V3 0 0 1

-- | Compute the eccentricity vector, see
-- https://en.wikipedia.org/wiki/Eccentricity_vector
eccentricityVector :: Floating a
                   => Quantity a [u| m^3 s^-2 |]
                   -> StateVectors a
                   -> V3 (Quantity a One)
eccentricityVector μ (StateVectors r v) = mk <$> e
  where e = (unQuantity <$> ((v `cross'` h) ^/: μ))
          - signorm (unQuantity <$> r)
        h = specificAngularMomentum (StateVectors r v)

-- | Calculate the specific kinetic energy, K, of a body.
specificKineticEnergy
  :: Floating a
  => StateVectors a -> Quantity a [u| m^2 s^-2 |]
specificKineticEnergy rv = speedSq rv /: 2

-- | Calculate the gravitational potential, V, of an orbiting body.
gravitationalPotential
  :: Floating a
  => Quantity a [u| m^3 s^-2 |]
  -> StateVectors a -> Quantity a [u| m^2 s^-2 |]
gravitationalPotential μ rv = negate' μ /: distance rv

-- | Calculate the specific mechanical energy of an orbiting body, E = K + V
specificMechanicalEnergy
  :: Floating a
  => Quantity a [u| m^3 s^-2 |] -> StateVectors a -> Quantity a [u| m^2 s^-2 |]
specificMechanicalEnergy μ rv =
  square (speed rv) /: 2 -: μ /: distance rv

--------------------------------------------------------------------------------
-- Extracting orbital parameters from state vectors
--------------------------------------------------------------------------------

inclinationFromStateVectors :: (Ord a, Floating a)
                            => StateVectors a -> InclinationSpecifier a
inclinationFromStateVectors rv =
  case ascendingNodeVector rv of
    Nothing -> NonInclined
    Just n  ->
      let h = specificAngularMomentum rv
          _Ω = let modΩ = acos' (n^._x / norm n)
               in if n^._y >= 0
                 then modΩ
                 else turn -: modΩ
          i = acos' (h^._z /: norm' h)
      in Inclined{ longitudeOfAscendingNode = _Ω
                 , inclination = i
                 }

-- | Calculate the semi-major axis length, a, of an orbit at the given state.
-- This returns 'Nothing' for a parabolic orbit.
semiMajorAxisFromStateVectors :: (Floating a, Eq a)
                              => Quantity a [u| m^3 s^-2 |]
                              -> StateVectors a
                              -> Maybe (Distance a)
semiMajorAxisFromStateVectors μ rv =
  let e = eccentricityVector μ rv
      _E = specificMechanicalEnergy μ rv
  in if quadrance' e == 1
       then Nothing
       else Just (negate' (μ /: (2 *: _E)))

-- | Calculate the periapsis, q, of an orbit from its state vectors
periapsisFromStateVectors :: (Floating a, Eq a)
                          => Quantity a [u| m^3 s^-2 |]
                          -> StateVectors a
                          -> Distance a
periapsisFromStateVectors μ rv =
  case semiMajorAxisFromStateVectors μ rv of
    Nothing -> let h = specificAngularMomentum rv
               in quadrance' h /: μ
    Just a  -> let e = eccentricityVector μ rv
               in a *: (1 - norm' e)

-- | Calculate the argument of periapsis, ω, of an orbit from its state vectors.
argumentOfPeriapsisFromStateVectors :: (Floating a, Ord a)
                                    => Quantity a [u| m^3 s^-2 |]
                                    -> StateVectors a
                                    -> PeriapsisSpecifier a
argumentOfPeriapsisFromStateVectors μ rv = case ascendingNodeVector rv of
  Nothing -> Circular
  Just n -> let e = eccentricityVector μ rv
            in Eccentric $ let modω = acos' ((n `dot'` e) /: norm' (n ^*^: e))
                           in if e^._z < 0
                                then turn -: modω
                                else modω

-- | Calculate the true anomaly of an orbit, ν, from its state vectors
trueAnomalyFromStateVectors :: forall a. (Floating a, Ord a)
                            => Quantity a [u| m^3 s^-2 |]
                            -> StateVectors a
                            -> Angle a
trueAnomalyFromStateVectors μ rv = ν
  where e = eccentricityVector μ rv
        r = position rv
        v = velocity rv
        ν = let modν = acos' ((e `dot'` r) /: norm' (e ^*^: r))
            in if r `dot'` v < [u|0m^2/s|]
                 then turn -: modν
                 else modν

--------------------------------------------------------------------------------
-- Rotations
--------------------------------------------------------------------------------

-- | Convert a point relative to the orbital plane into one relative to the
-- global reference frame.
--
-- @∀ o. planeToWorld o . worldToPlane o = id@
planeToWorld :: (RealFloat a, Conjugate a)
             => Orbit a -> V3 (Distance a) -> V3 (Distance a)
planeToWorld o = fmap MkQuantity
               . rotate (orbitalPlaneQuaternion o)
               . fmap unQuantity

-- | Convert a point relative to the global reference frame into one relative
-- to the orbital plane
--
-- @∀ o. planeToWorld o . worldToPlane o = id@
worldToPlane :: (RealFloat a, Conjugate a)
             => Orbit a -> V3 (Distance a) -> V3 (Distance a)
worldToPlane o = fmap MkQuantity
               . rotate (conjugate (orbitalPlaneQuaternion o))
               . fmap unQuantity

-- | A quaternion representing the rotation into the orbital plane.
--
-- It first performs any rotation in the orbital plane, and then inclines the
-- plane.
orbitalPlaneQuaternion :: RealFloat a => Orbit a -> Quaternion a
orbitalPlaneQuaternion o = inclinationQuat * inPlaneQuat
  where inPlaneQuat = case periapsisSpecifier o of
          Eccentric ω -> rotateZ ω
          Circular    -> noRotation
        inclinationQuat = case inclinationSpecifier o of
          Inclined{..} -> rotateZ longitudeOfAscendingNode * rotateX inclination
          NonInclined -> noRotation

--------------------------------------------------------------------------------
-- Elements from state vectors
--------------------------------------------------------------------------------

distanceAtTrueAnomaly :: (Ord a, Floating a)
                      => Orbit a -> Angle a -> Distance a
distanceAtTrueAnomaly o ν = case semiMajorAxis o of
  -- Parabolic orbit
  Nothing -> let h = h in undefined
  -- Elliptic or hyperbolic
  Just a -> let e = eccentricity o
            in a *: (1 - square e) /: (1 + e * cos' ν)

positionAtTrueAnomaly = positionAtTrueAnomaly

--------------------------------------------------------------------------------
-- Converting between all elements
--------------------------------------------------------------------------------

-- | Calculate all of an orbits elements from a set of state vectors
orbitFromStateVectors :: (Ord a, Floating a)
                      => Quantity a [u| m^3 s^-2 |] -> StateVectors a -> Orbit a
orbitFromStateVectors μ rv =
  let inc = inclinationFromStateVectors rv
      e   = eccentricityVector μ rv
      per = argumentOfPeriapsisFromStateVectors μ rv
      q   = periapsisFromStateVectors μ rv
  in Orbit{ eccentricity = norm' e
          , periapsis = q
          , inclinationSpecifier = inc
          , periapsisSpecifier = per
          , primaryGravitationalParameter = μ
          }

-- | Create a function mapping time, t, to state vectors for an orbit
stateVectorsFromOrbitAtTime :: Orbit a -> Time a -> StateVectors a
stateVectorsFromOrbitAtTime = undefined

{-

radiusAtTrueAnomaly :: Orbit -> Angle -> Distance
radiusAtTrueAnomaly orbit trueAnomaly = a * (1 - e^2) / (1 + e * cos ν)
  where e = eccentricity orbit
        a = semiMajorAxis orbit
        ν = trueAnomaly

speedAtTrueAnomaly :: Orbit -> Angle -> Double
speedAtTrueAnomaly orbit trueAnomaly = sqrt (μ * (2 / r - 1 / a))
  where ν = trueAnomaly
        μ = primaryGravitationalParameter orbit
        r = radiusAtTrueAnomaly orbit ν
        a = semiMajorAxis orbit

positionAtTrueAnomaly :: Orbit -> Angle -> V3 Double
positionAtTrueAnomaly orbit trueAnomaly = rotateToWorld orbit r
  where ν = trueAnomaly
        d = radiusAtTrueAnomaly orbit ν
        r = V3 (cos ν) (sin ν) 0 ^* d

velocityAtTrueAnomaly :: Orbit -> Angle -> V3 Double
velocityAtTrueAnomaly orbit trueAnomaly = rotateToWorld orbit v
  where ν = trueAnomaly
        μ = primaryGravitationalParameter orbit
        e = eccentricity orbit
        h = sqrt (μ * a * (1 - e^2))
        a = semiMajorAxis orbit
        r = radiusAtTrueAnomaly orbit trueAnomaly
        vr = μ * e * sin ν / h
        vtA = h / r
        v = V3 (vr * cos ν - vtA * sin ν) (vr * sin ν + vtA * cos ν) 0

trueAnomalyAtPosition :: Orbit -> V3 Double -> Angle
trueAnomalyAtPosition orbit r = ν
  where V3 x y _ = rotateToPlane orbit r
        ν = atan2 y x
-}
