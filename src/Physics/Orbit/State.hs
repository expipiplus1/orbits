-- Extensions for uom-plugin
{-# LANGUAGE DataKinds           #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiWayIf #-}
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

    -- ** Conversions between all elements
  , orbitFromStateVectors
  , stateVectorsFromTrueAnomaly

    -- ** Utilities
  , isValid
  , orbitalDirection
  , LinePass(..)
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
  , positionInPlaneAtTrueAnomaly
  , positionAtTrueAnomaly
  , speedAtTrueAnomaly
  , radialVelocityAtTrueAnomaly
  , tangentialVelocityAtTrueAnomaly
  , velocityInPlaneAtTrueAnomaly
  , velocityAtTrueAnomaly

    -- ** Rotations between reference frames
  , planeToWorld
  , worldToPlane
  , orbitalPlaneQuaternion
  ) where

import Data.UnitsOfMeasure.Defs     ()
import Data.UnitsOfMeasure.Extra
import Data.UnitsOfMeasure.Internal (Quantity (..))
import Lens.Micro                   ((^.))
import Linear.Conjugate
import Linear.Metric                hiding (distance)
import Linear.Quaternion.Extra      hiding (Angle)
import Linear.V3
import Linear.Plucker(LinePass(..))
import Physics.Orbit                hiding (isValid)
import Physics.Radian

{-# ANN module "HLint: Reduce Duplication" #-}

-- | A position and velocity of an orbiting body.
data StateVectors a = StateVectors
  { position :: Position a
  , velocity :: Velocity a
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Is this set of state vectors valid. They are invalid if they represent a
-- degenerate orbit with a velocity of zero or a position of zero.
isValid :: (Eq a, Num a) => StateVectors a -> Bool
isValid (StateVectors r v) = r /= pure [u|0m|] && v /= pure [u|0m/s|]

-- | 'orbitalDirection' returns the direction of the orbital axis around the
-- universal Z axis. It returns 'CoPlanar' if the orbit has an inclination of
-- 90 degrees.
orbitalDirection :: (Ord a, Num a) => StateVectors a -> LinePass
orbitalDirection rv = if | hz < [u|0m^2/s|]  -> Clockwise  
                         | hz > [u|0m^2/s|]  -> Counterclockwise
                         | hz == [u|0m^2/s|] -> Coplanar
  where h = specificAngularMomentum rv
        hz = h^._z

-- | 'distance' returns the distance of the orbiting body from the center of
-- the primary body.
distance :: Floating a => StateVectors a -> Distance a
distance (StateVectors r _) = norm' r

-- | 'speed' returns the speed of the orbiting body relative to the primary
-- body.
speed :: Floating a => StateVectors a -> Speed a
speed (StateVectors _ v) = norm' v

-- | 'speedSq' returns the square of the speed of orbiting body relative to the
-- primary body.
speedSq :: Floating a => StateVectors a -> Quantity a [u|m^2 s^-2|]
speedSq (StateVectors _ v) = quadrance' v

-- | Compute the specific angular momentum, h, of an orbiting body from its
-- state vectors.
specificAngularMomentum :: Num a
                        => StateVectors a
                        -> V3 (Quantity a [u|m^2/s|])
specificAngularMomentum (StateVectors r v) = h
  where h = r `cross'` v

-- | Return 'True' if the given state vectors represent an inclined orbit.
isInclined :: (Eq a, Num a) => StateVectors a -> Bool
isInclined (StateVectors r v) = r^._z /= [u|0m|] || v^._z /= [u|0m/s|]

-- | Compute the vector pointing towards the ascening node of an orbit from its
-- state vectors. If the orbit is not inclined then this function returns
-- 'Nothing'.
ascendingNodeVector :: (Eq a, Num a)
                    => StateVectors a
                    -> Maybe (V3 (Quantity a One))
ascendingNodeVector rv
  | isInclined rv = Just (mk <$> n)
  | otherwise     = Nothing
  where n = k `cross` (unQuantity <$> h)
        h = specificAngularMomentum rv
        k = V3 0 0 1

-- | Compute the eccentricity vector, e. see
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
    Nothing -> error "equatorial"
    Just n  ->
      let h = specificAngularMomentum rv
          _Ω = let modΩ = acos' (n^._x / norm n)
               in if n^._y >= 0
                 then modΩ
                 else turn -: modΩ
          i = acos' (h^._z /: norm' h)
      in Inclined{ longitudeOfAscendingNode = _Ω
                 , nonEquatorialInclination = i
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
               in quadrance' h /: (2 *: μ)
    Just a  -> let e = eccentricityVector μ rv
               in a *: (1 - norm' e)

-- | Calculate the argument of periapsis, ω, of an orbit from its state vectors.
argumentOfPeriapsisFromStateVectors :: (RealFloat a, Ord a)
                                    => Quantity a [u| m^3 s^-2 |]
                                    -> StateVectors a
                                    -> PeriapsisSpecifier a
argumentOfPeriapsisFromStateVectors μ rv = 
  let e = eccentricityVector μ rv
  in if e == 0
       then Circular
       else Eccentric $ case ascendingNodeVector rv of
              Nothing -> let ωDir = atan2' (e^._y) (e^._x)
                         in if orbitalDirection rv == Clockwise
                              then turn -: ωDir
                              else ωDir
              Just n -> let modω = acos' ((n `dot'` e) /: (norm' n *: norm' e))
                        in if e^._z < 0
                             then turn -: modω
                             else modω

-- | Calculate the true anomaly of an orbit, ν, from its state vectors
trueAnomalyFromStateVectors :: forall a. (Floating a, Ord a, Show a)
                            => Quantity a [u| m^3 s^-2 |]
                            -> StateVectors a
                            -> Angle a
trueAnomalyFromStateVectors μ rv = ν
  where e = eccentricityVector μ rv
        r = position rv
        v = velocity rv
        ν = let modν = acos' ((e `dot'` r) /: (norm' e *: norm' r))
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
             => Orbit a
             -> V3 (Quantity a ([u|m|] *: u)) -> V3 (Quantity a ([u|m|] *: u))
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
          Inclined{..}         -> rotateZ longitudeOfAscendingNode 
                                * rotateX nonEquatorialInclination
          EquatorialPrograde   -> noRotation
          EquatorialRetrograde -> rotateX (negate' halfTurn)

--------------------------------------------------------------------------------
-- Elements from state vectors
--------------------------------------------------------------------------------

distanceAtTrueAnomaly :: (Ord a, Floating a)
                      => Orbit a -> Angle a -> Distance a
distanceAtTrueAnomaly o ν = r
  where r = l /: (1 + e * cos' ν)
        l = semiLatusRectum o
        e = eccentricity o

positionInPlaneAtTrueAnomaly ::
  (RealFloat a, Conjugate a) => Orbit a -> Angle a -> V3 (Distance a)
positionInPlaneAtTrueAnomaly o ν = rPlane
  where rPlane = V3 (cos' ν *: r) (sin' ν *: r) [u|0m|]
        r = distanceAtTrueAnomaly o ν

positionAtTrueAnomaly ::
  (RealFloat a, Conjugate a) =>
  Orbit a -> Angle a -> V3 (Distance a)
positionAtTrueAnomaly o ν = planeToWorld o (positionInPlaneAtTrueAnomaly o ν)

speedAtTrueAnomaly ::
  (Floating a, Ord a) =>
  Orbit a -> Angle a -> Speed a
speedAtTrueAnomaly o ν = case classify o of
  Parabolic -> sqrt' (2 *: μ /: r)
  _         -> sqrt' (μ *: (2 /: r -: 1 /: a))
    where Just a = semiMajorAxis o
  where μ = primaryGravitationalParameter o
        r = distanceAtTrueAnomaly o ν

radialVelocityAtTrueAnomaly :: Floating a => Orbit a -> Angle a -> Speed a
radialVelocityAtTrueAnomaly o ν = vRadial
  where vRadial = sqrt' (μ /: l) *: e *: sin' ν
        l = semiLatusRectum o
        μ = primaryGravitationalParameter o
        e = eccentricity o

tangentialVelocityAtTrueAnomaly :: Floating a => Orbit a -> Angle a -> Speed a
tangentialVelocityAtTrueAnomaly o ν = vTangent
  where vTangent = sqrt' (μ /: l) *: (1 + e *: cos' ν)
        l = semiLatusRectum o
        μ = primaryGravitationalParameter o
        e = eccentricity o

velocityInPlaneAtTrueAnomaly ::
  Floating a =>
  Orbit a -> Angle a -> V3 (Speed a)
velocityInPlaneAtTrueAnomaly o ν = V3 vX vY [u|0m/s|]
  where n = sqrt' (μ /: l)
        l = semiLatusRectum o
        μ = primaryGravitationalParameter o
        e = eccentricity o
        vX = negate' n *: sin' ν
        vY = n *: (e + cos' ν)

velocityAtTrueAnomaly ::
  (RealFloat a, Conjugate a) =>
  Orbit a
  -> Angle a
  -> V3 (Speed a)
velocityAtTrueAnomaly o ν = planeToWorld o (velocityInPlaneAtTrueAnomaly o ν)

--------------------------------------------------------------------------------
-- Converting between all elements
--------------------------------------------------------------------------------

-- | Calculate all of an orbits elements from a set of state vectors
orbitFromStateVectors :: (Ord a, RealFloat a)
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

-- | Calculate the state vectors when the orbit is at a given true anomaly, ν
stateVectorsFromTrueAnomaly ::
  (RealFloat a, Conjugate a) => Orbit a -> Angle a -> StateVectors a
stateVectorsFromTrueAnomaly o ν = StateVectors r v
  where r = positionAtTrueAnomaly o ν
        v = velocityAtTrueAnomaly o ν
