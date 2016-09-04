-- Extensions for uom-plugin
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- | Types and functions for dealing with Kepler orbits.
module Physics.Orbit.Anomaly
  ( -- ** Conversions between different anomalies

    -- *** To time since periapse
    timeAtMeanAnomaly
  , timeAtEccentricAnomaly
  , timeAtTrueAnomaly

    -- *** To mean anomaly
  , meanAnomalyAtTime
  , meanAnomalyAtEccentricAnomaly
  , meanAnomalyAtTrueAnomaly

    -- *** To eccentric anomaly
  , eccentricAnomalyAtTime
  , eccentricAnomalyAtMeanAnomaly
  , eccentricAnomalyAtMeanAnomalyFloat
  , eccentricAnomalyAtTrueAnomaly

    -- *** To true anomaly
  , trueAnomalyAtTime
  , trueAnomalyAtMeanAnomaly
  , trueAnomalyAtEccentricAnomaly

    -- * Reexported from 'Data.CReal'
  , Converge
  ) where

import Physics.Orbit
import Control.Monad                ((<=<))
import Data.Bifunctor               (bimap, second)
import Data.CReal.Converge          (Converge, convergeErr)
import Data.UnitsOfMeasure.Extra
import Numeric.AD                   (Mode, Scalar, auto)
import Numeric.AD.Halley            (findZero, findZeroNoEq)
import Numeric.AD.Internal.Identity (Id (..))
import Physics.Radian               (halfTurn, turn)

-- | Calculate the time since periapse, t, when the body has the given
-- <https://en.wikipedia.org/wiki/Mean_anomaly mean anomaly>, M. M may be
-- negative, indicating that the orbiting body has yet to reach periapse.
--
-- The sign of the time at mean anomaly M is the same as the sign of M.
--
-- The returned time is unbounded.
timeAtMeanAnomaly :: (Floating a, Ord a) => Orbit a -> Angle a -> Time a
timeAtMeanAnomaly o _M = _M /: n
  where n = meanMotion o

-- | Calculate the time since periapse, t, of an elliptic orbit when at
-- eccentric anomaly E.
--
-- 'timeAtEccentricAnomaly' returns Nothing if given a parabolic or hyperbolic
-- orbit.
timeAtEccentricAnomaly :: (Floating a, Ord a) => Orbit a -> Angle a -> Maybe (Time a)
timeAtEccentricAnomaly o = fmap (timeAtMeanAnomaly o) . meanAnomalyAtEccentricAnomaly o

-- | Calculate the time since periapse given the true anomaly, ν, of an
-- orbiting body.
timeAtTrueAnomaly :: (Real a, Floating a) => Orbit a -> Angle a -> Maybe (Time a)
timeAtTrueAnomaly o = fmap (timeAtMeanAnomaly o) . meanAnomalyAtTrueAnomaly o

-- | Calculate the <https://en.wikipedia.org/wiki/Mean_anomaly mean anomaly>,
-- M, at the given time since periapse, t. t may be negative, indicating that
-- the orbiting body has yet to reach periapse.
--
-- The sign of the mean anomaly at time t is the same as the sign of t.
--
-- The returned mean anomaly is unbounded.
meanAnomalyAtTime :: (Floating a, Ord a) => Orbit a -> Time a -> Angle a
meanAnomalyAtTime o t = t *: n
  where n = meanMotion o

-- | Calculate the mean anomaly, M, of an elliptic orbit when at eccentric
-- anomaly E
--
-- 'meanAnomalyAtEccentricAnomaly' returns Nothing if given a parabolic or
-- hyperbolic orbit.
--
-- The number of orbits represented by the anomalies is preserved;
-- i.e. M `div` 2π = E `div` 2π
meanAnomalyAtEccentricAnomaly :: (Floating a, Ord a) => Orbit a -> Angle a -> Maybe (Angle a)
meanAnomalyAtEccentricAnomaly o _E = case classify o of
                                       Elliptic -> Just _M
                                       _ -> Nothing
  where e = eccentricity o
        untypedE = convert _E
        _M = convert (untypedE -: e *: sin untypedE)

-- | Calculate the mean anomaly, M, of an orbiting body when at the given true
-- anomaly, ν.
--
-- The number of orbits represented by the anomalies is preserved;
-- i.e. M `div` 2π = ν `div` 2π
--
-- Currently only implemented for elliptic orbits.
meanAnomalyAtTrueAnomaly :: (Real a, Floating a)
                         => Orbit a -> Angle a -> Maybe (Angle a)
meanAnomalyAtTrueAnomaly o = case classify o of
  Elliptic -> meanAnomalyAtEccentricAnomaly o <=<
              eccentricAnomalyAtTrueAnomaly o
  _ -> error "TODO: meanAnomalyAtTrueAnomaly"

-- | Calculate the eccentric anomaly, E, of an elliptic orbit at time t.
--
-- 'eccentricAnomalyAtTime' returns Nothing when given a parabolic or
-- hyperbolic orbit.
--
-- The number of orbits represented by the time is preserved;
-- i.e. t `div` p = E `div` 2π
eccentricAnomalyAtTime :: (Converge [a], Floating a, Real a)
                       => Orbit a -> Time a -> Maybe (Angle a)
eccentricAnomalyAtTime o t = case classify o of
  Elliptic -> eccentricAnomalyAtMeanAnomaly o . meanAnomalyAtTime o $ t
  _ -> Nothing

-- | Calculate the eccentric anomaly, E, of an elliptic orbit when at mean
-- anomaly M. This function is considerably slower than most other conversion
-- functions as it uses an iterative method as no closed form solution exists.
--
-- The number of orbits represented by the anomalies is preserved;
-- i.e. M `div` 2π = E `div` 2π
--
-- 'eccentricAnomalyAtMeanAnomaly' returns Nothing when given a parabolic or
-- hyperbolic orbit.
eccentricAnomalyAtMeanAnomaly :: forall a. (Converge [a], Floating a, Real a)
                              => Orbit a -> Angle a -> Maybe (Angle a)
eccentricAnomalyAtMeanAnomaly o _M = case classify o of
                                       Elliptic -> _E
                                       _ -> Nothing
  where (n, wrappedM) = second unQuantity (_M `divMod'` turn)
        e = unQuantity (eccentricity o)
        _MFloat = [u|rad|] . realToFrac $ wrappedM
        oFloat = unsafeMapOrbit realToFrac o
        initialGuessFloat :: Angle Float
        Just initialGuessFloat = eccentricAnomalyAtMeanAnomalyFloat oFloat _MFloat
        initialGuess = realToFrac . unQuantity $ initialGuessFloat
        err :: (Mode b, Floating b, Scalar b ~ a) => b -> b
        err _E = auto wrappedM - (_E - auto e * sin _E)
        wrappedE = fmap [u|rad|] . convergeErr (runId . abs . err .  Id) $
                   findZeroNoEq err initialGuess
        _E = (+: (unsafeMapUnit fromInteger n *: turn)) <$> wrappedE

-- | 'eccentricAnomalyAtMeanAnomaly' specialized to 'Float'.
--
-- This function is used to calculate the initial guess for
-- 'eccentricAnomalyAtMeanAnomaly'.
eccentricAnomalyAtMeanAnomalyFloat :: Orbit Float -> Angle Float -> Maybe (Angle Float)
eccentricAnomalyAtMeanAnomalyFloat o _M = case classify o of
                                            Elliptic -> Just _E
                                            _ -> Nothing
  where wrappedM = unQuantity (_M `mod'` turn)
        e = unQuantity (eccentricity o)
        sinM = sin wrappedM
        cosM = cos wrappedM
        -- Use a better initial guess
        -- http://alpheratz.net/dynamics/twobody/KeplerIterations_summary.pdf
        initialGuess = wrappedM +
                       e * sinM +
                       e * e * sinM * cosM +
                       0.5 * e * e * e * sinM * (3 * cosM * cosM - 1)
        _E :: Angle Float
        _E = [u|rad|] . last . take 5 $
             findZero (\_E -> auto wrappedM - (_E - auto e * sin _E))
                      initialGuess

-- | Calculate the eccentric anomaly, E, of an orbiting body when it has true
-- anomaly, ν.
--
-- The number of orbits represented by the anomalies is preserved;
-- i.e. ν `div` 2π = E `div` 2π
--
-- Returns Nothing if given a parabolic or hyperbolic orbit.
eccentricAnomalyAtTrueAnomaly :: (Floating a, Real a)
                              => Orbit a -> Angle a -> Maybe (Angle a)
eccentricAnomalyAtTrueAnomaly o ν = case classify o of
                                       Elliptic -> Just _E
                                       _ -> Nothing
  where (n, wrappedν) = ν `divMod'` turn
        cosν = cos (unQuantity ν)
        -- sinν = sin (unQuantity wrappedν)
        e = unQuantity (eccentricity o)
        wrappedE = [u|rad|] $ acos ((e + cosν) / (1 + e * cosν))
        -- wrappedE = [u|rad|] $ atan2 (sqrt (1 - e*e) * sinν) (e + cosν)
        _E = if wrappedν < halfTurn
               then (unsafeMapUnit fromInteger n *: turn) +: wrappedE
               else (unsafeMapUnit fromInteger (n+1) *: turn) -: wrappedE

-- | Calculate the true anomaly, ν, of a body at time since periapse, t.
trueAnomalyAtTime :: (Converge [a], RealFloat a)
                  => Orbit a -> Time a -> Maybe (Angle a)
trueAnomalyAtTime o = trueAnomalyAtMeanAnomaly o . meanAnomalyAtTime o

-- | Calculate the true anomaly, ν, of an orbiting body when it has the given
-- mean anomaly, _M.
trueAnomalyAtMeanAnomaly :: (Converge [a], RealFloat a)
                         => Orbit a -> Angle a -> Maybe (Angle a)
trueAnomalyAtMeanAnomaly o = trueAnomalyAtEccentricAnomaly o <=<
                             eccentricAnomalyAtMeanAnomaly o

-- | Calculate the true anomaly, ν, of an orbiting body when it has the given
-- eccentric anomaly, _E.
--
-- The number of orbits represented by the anomalies is preserved;
-- i.e. ν `div` 2π = E `div` 2π
trueAnomalyAtEccentricAnomaly :: RealFloat a
                              => Orbit a -- ^ An elliptic orbit
                              -> Angle a -- ^ The eccentric anomaly _E
                              -> Maybe (Angle a) -- ^ The true anomaly, ν
trueAnomalyAtEccentricAnomaly o _E = case classify o of
                                       Elliptic -> Just ν
                                       _        -> Nothing
  where (n, wrappedE) = bimap (unsafeMapUnit fromInteger) unQuantity $
                        _E `divMod'` turn
        e = unQuantity $ eccentricity o
        wrappedν = [u|rad|] $ 2 * atan2 (sqrt (1 + e) * sin (wrappedE / 2))
                                        (sqrt (1 - e) * cos (wrappedE / 2))
        ν = turn *: n +: wrappedν

