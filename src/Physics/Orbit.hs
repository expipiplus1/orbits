{-# language QuasiQuotes #-}

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
  , normalizeOrbit
    -- ** Orbital elements
  , apoapsis
  , meanMotion
  , period
  , arealVelocity
    -- *** Geometry
  , semiMajorAxis
  , semiMinorAxis
  , semiLatusRectum
  , hyperbolicApproachAngle
  , hyperbolicDepartureAngle
    -- ** Conversions

    -- *** To time since periapse
  , timeAtMeanAnomaly
  , timeAtEccentricAnomaly
  , timeAtHyperbolicAnomaly
  , timeAtTrueAnomaly

    -- *** To mean anomaly
  , meanAnomalyAtTime
  , meanAnomalyAtEccentricAnomaly
  , meanAnomalyAtHyperbolicAnomaly
  , meanAnomalyAtTrueAnomaly

    -- *** To eccentric anomaly
  , eccentricAnomalyAtTime
  , eccentricAnomalyAtMeanAnomaly
  , eccentricAnomalyAtMeanAnomalyFloat
  , eccentricAnomalyAtTrueAnomaly

    -- *** To hyperbolic anomaly
  , hyperbolicAnomalyAtTime
  , hyperbolicAnomalyAtMeanAnomaly
  , hyperbolicAnomalyAtMeanAnomalyDouble
  , hyperbolicAnomalyAtTrueAnomaly

    -- *** To true anomaly
  , trueAnomalyAtTime
  , trueAnomalyAtMeanAnomaly
  , trueAnomalyAtEccentricAnomaly
  , trueAnomalyAtHyperbolicAnomaly

    -- *** Properties of orbits
  , specificAngularMomentum
  , specificOrbitalEnergy
  , specificPotentialEnergyAtTrueAnomaly
  , specificKineticEnergyAtTrueAnomaly
  , speedAtTrueAnomaly
  , radiusAtTrueAnomaly

    -- *** Other utilities
  , escapeVelocityAtDistance

    -- * Unit synonyms
  , Quantity
  , Time
  , Distance
  , Speed
  , Mass
  , Angle
  , AngleH
  , RadianHyperbolic(..)
  , PlaneAngleHyperbolic(..)
  , Unitless

    -- * Reexported from 'Data.CReal'
  , Converge
  ) where

import           Control.Monad                  ( (<=<) )
import           Data.Bifunctor                 ( bimap
                                                , second
                                                )
import           Data.CReal.Converge            ( Converge
                                                , convergeErr
                                                )
import           Data.Constants.Mechanics.Extra
import           Data.Maybe                     ( fromJust )
import           Data.Metrology
import           Data.Metrology.Extra
import           Data.Metrology.Show            ( )
import           Data.Metrology.Unsafe          ( Qu(..)
                                                , UnsafeQu(..)
                                                )
import           Data.Units.SI.Parser
import           Numeric.AD                     ( Mode
                                                , Scalar
                                                , auto
                                                )
import           Numeric.AD.Halley              ( findZero
                                                , findZeroNoEq
                                                )
import           Numeric.AD.Internal.Identity   ( Id(..) )
import qualified Numeric.AD.Newton.Double      as Newton
import           Physics.Orbit.Metrology

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

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
                       eccentricity                  :: !(Unitless a)
                       -- | The orbit's periapsis, q.
                       --
                       -- 'periapsis' must be positive.
                       --
                       -- The periapsis is the distance between the bodies at
                       -- their closest approach.
                     , periapsis                     :: !(Distance a)
                       -- | The 'inclinationSpecifier' describes the angle
                       -- between the obtital plane and the reference plane.
                     , inclinationSpecifier          :: !(InclinationSpecifier a)
                       -- | 'periapsisSpecifier' is 'Circular' iff
                       -- 'eccentricity' is 0
                       --
                       -- The periapsis specifier describes any rotation of
                       -- the orbit relative to the reference direction in the
                       -- orbital plane.
                     , periapsisSpecifier            :: !(PeriapsisSpecifier a)
                       -- | The gravitational parameter of the system's
                       -- primary, μ.
                       --
                       -- μ is equal to the mass of the primary times
                       -- <https://en.wikipedia.org/wiki/Gravitational_constant
                       -- G>.
                       --
                       -- 'primaryGravitationalParameter' must be positive.
                     , primaryGravitationalParameter :: !(Quantity [si| m^3 s^-2 |] a)
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
                                       , inclination              :: !(Angle a)
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
                                        argumentOfPeriapsis :: !(Angle a)
                                      }
                            -- | The orbit has an eccentricity of 0 so the
                            -- 'argumentOfPeriapsis' is indeterminate.
                          | Circular
  deriving (Show, Eq)

-- | What form the orbit's geometry takes. This is dependant only on the
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

-- TODO, use the neat "UnsafeQu" newtype for unsafe instances
unsafeMapUnit :: (a -> b) -> Qu u l a -> Qu u l b
unsafeMapUnit f = qu . fmap f . UnsafeQu

unsafeMapOrbit :: (a -> b) -> Orbit a -> Orbit b
unsafeMapOrbit f (Orbit e q i p μ) = Orbit (unsafeMapUnit f e)
                                           (unsafeMapUnit f q)
                                           (unsafeMapInclinationSpecifier f i)
                                           (unsafeMapPeriapsisSpecifier f p)
                                           (unsafeMapUnit f μ)

unsafeMapInclinationSpecifier :: (a -> b)
                              -> InclinationSpecifier a -> InclinationSpecifier b
unsafeMapInclinationSpecifier f s = case s of
  Inclined _Ω i -> Inclined (unsafeMapUnit f _Ω) (unsafeMapUnit f i)
  NonInclined   -> NonInclined

unsafeMapPeriapsisSpecifier :: (a -> b)
                            -> PeriapsisSpecifier a -> PeriapsisSpecifier b
unsafeMapPeriapsisSpecifier f p = case p of
  Circular    -> Circular
  Eccentric a -> Eccentric (unsafeMapUnit f a)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Determines if the orbital elements are valid (@e >= 0@ etc...). The
-- behavior of all the other functions in this module is undefined when given
-- an invalid orbit.
isValid :: (Ord a, Num a) => Orbit a -> Bool
isValid o = e >= 0 &&
            ((e == 0) `iff` (periapsisSpecifier o == Circular)) &&
            q > zero &&
            μ > zero
  where
    iff = (==) :: Bool -> Bool -> Bool
    e = eccentricity o
    q = periapsis o
    μ = primaryGravitationalParameter o

-- | What shape is the orbit
classify :: (Num a, Ord a) => Orbit a -> Classification
classify o | e < 1     = Elliptic
           | e == 1    = Parabolic
           | e > 1     = Hyperbolic
           | otherwise = error "classify: NaN eccentricity"
  where e = eccentricity o

-- | Return an equivalent orbit such that
--
-- - i ∈ [0..π)
-- - Ω ∈ [0..2π)
-- - ω ∈ [0..2π)
-- - inclinationSpecifier == NonInclined if i = 0
-- - periapsisSpecifier == Circular if e == 0 and ω == 0
normalizeOrbit :: (Floating a, Real a) => Orbit a -> Orbit a
normalizeOrbit (Orbit e q inc per μ) = Orbit e q inc' per' μ
 where
  -- Were we actually given a descending node and have to flip things
  (inc', flipped) = case inc of
    NonInclined              -> (NonInclined, False)
    Inclined _ i | i == zero -> (NonInclined, False)
    Inclined _Ω i ->
      let iR  = i `mod'` turn
          i'  = if flipped then turn |-| iR else iR
          _Ω' = (if flipped then _Ω |+| halfTurn else _Ω) `mod'` turn
      in  (Inclined _Ω' i', iR >= halfTurn)

  per' = case per of
    Circular | flipped   -> Eccentric halfTurn
             | otherwise -> Circular
    Eccentric ω
      | ω == zero, e == 0, not flipped
      -> Circular
      | otherwise
      -> Eccentric $ (if flipped then ω |+| halfTurn else ω) `mod'` turn

-- | Calculate the semi-major axis, a, of the 'Orbit'. Returns 'Nothing' when
-- given a parabolic orbit for which there is no semi-major axis. Note that the
-- semi-major axis of a hyperbolic orbit is negative.
semiMajorAxis :: (Fractional a, Ord a) => Orbit a -> Maybe (Distance a)
semiMajorAxis o =
  case classify o of
    Parabolic -> Nothing
    _         -> Just $ q |/| (1 |-| e)
  where
    q = periapsis o
    e = eccentricity o

-- | Calculate the semi-minor axis, b, of the 'Orbit'. Like 'semiMajorAxis'
-- @\'semiMinorAxis\' o@ is negative when @o@ is a hyperbolic orbit. In the
-- case of a parabolic orbit 'semiMinorAxis' returns @0m@.
semiMinorAxis :: (Floating a, Ord a) => Orbit a -> Distance a
semiMinorAxis o =
  case classify o of
    Elliptic   -> a |*| qSqrt (1 |-| e ^ (2::Int))
    Parabolic  -> zero
    Hyperbolic -> a |*| qSqrt (e ^ (2::Int) |-| 1)
  where
    e = eccentricity o
    Just a = semiMajorAxis o

-- | Calculate the semiLatusRectum, l, of the 'Orbit'
semiLatusRectum :: (Num a) => Orbit a -> Distance a
semiLatusRectum orbit = e |*| q |+| q
  where q = periapsis orbit
        e = eccentricity orbit

-- | Calculate the distance between the bodies when they are at their most
-- distant. 'apoapsis' returns 'Nothing' when given a parabolic or hyperbolic
-- orbit.
apoapsis :: (Fractional a, Ord a) => Orbit a -> Maybe (Distance a)
apoapsis o =
  case classify o of
    Elliptic -> Just $ a |*| (1 |+| e)
    _        -> Nothing
  where
    Just a = semiMajorAxis o
    e = eccentricity o

-- | Calculate the mean motion, n, of an orbit
--
-- This is the rate of change of the mean anomaly with respect to time.
meanMotion :: (Floating a, Ord a) => Orbit a -> Quantity [si|rad/s|] a
meanMotion o =
  case classify o of
    Elliptic   -> addRad $ qSqrt (μ |/| qCube a)
    Hyperbolic -> addRad $ qSqrt (μ |/| qNegate (qCube a))
    Parabolic  -> addRad $ 2 |*| qSqrt (μ |/| qCube l)
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
    p = turn |/| n


-- | Calculate the areal velocity, A, of the orbit.
--
-- The areal velocity is the area <https://xkcd.com/21/ swept out> by the line
-- between the orbiting body and the primary per second.
arealVelocity :: (Ord a, Floating a) => Orbit a -> Quantity [si|m^2/s|] a
arealVelocity o = qSqrt (l |*| μ) |/| 2
  where l = semiLatusRectum o
        μ = primaryGravitationalParameter o

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
          θ = addRad $ acos (-1 / e)
      in Just θ
    Parabolic -> Just (turn |/| 2)
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
hyperbolicApproachAngle = fmap qNegate . hyperbolicDepartureAngle

----------------------------------------------------------------
-- ## Conversions between time and anomolies
----------------------------------------------------------------

---------
-- To time
---------

-- | Calculate the time since periapse, t, when the body has the given
-- <https://en.wikipedia.org/wiki/Mean_anomaly mean anomaly>, M. M may be
-- negative, indicating that the orbiting body has yet to reach periapse.
--
-- The sign of the time at mean anomaly M is the same as the sign of M.
--
-- The returned time is unbounded.
timeAtMeanAnomaly :: (Floating a, Ord a) => Orbit a -> Angle a -> Time a
timeAtMeanAnomaly o _M = _M |/| n
  where n = meanMotion o

-- | Calculate the time since periapse, t, of an elliptic orbit when at
-- eccentric anomaly E.
--
-- 'timeAtEccentricAnomaly' returns Nothing if given a parabolic or hyperbolic
-- orbit.
timeAtEccentricAnomaly :: (Floating a, Ord a) => Orbit a -> Angle a -> Maybe (Time a)
timeAtEccentricAnomaly o = fmap (timeAtMeanAnomaly o) . meanAnomalyAtEccentricAnomaly o

-- | Calculate the time since periapse, t, of a hyperbolic orbit when at
-- hyperbolic anomaly H.
--
-- Returns Nothing if given an elliptic or parabolic orbit.
timeAtHyperbolicAnomaly
  :: (Floating a, Ord a) => Orbit a -> AngleH a -> Maybe (Time a)
timeAtHyperbolicAnomaly o =
  fmap (timeAtMeanAnomaly o) . meanAnomalyAtHyperbolicAnomaly o

-- | Calculate the time since periapse given the true anomaly, ν, of an
-- orbiting body.
--
-- Returns 'Nothing' if the body never passed through the specified true
-- anomaly.
timeAtTrueAnomaly
  :: (Real a, Floating a) => Orbit a -> Angle a -> Maybe (Time a)
timeAtTrueAnomaly o ν = case classify o of
  _ | Just d <- hyperbolicDepartureAngle o, qAbs ν |>| d -> Nothing
  Parabolic ->
    let _D = qTan (ν |/| 2)
        t  = 0.5 |*| qSqrt (qCube l |/| μ) |*| (_D |+| (qCube _D |/| 3))
    in  Just t
  _ -> fmap (timeAtMeanAnomaly o) . meanAnomalyAtTrueAnomaly o $ ν
 where
  μ = primaryGravitationalParameter o
  l = semiLatusRectum o

---------
-- To mean anomaly
---------

-- | Calculate the <https://en.wikipedia.org/wiki/Mean_anomaly mean anomaly>,
-- M, at the given time since periapse, t. t may be negative, indicating that
-- the orbiting body has yet to reach periapse.
--
-- The sign of the mean anomaly at time t is the same as the sign of t.
--
-- The returned mean anomaly is unbounded.
meanAnomalyAtTime :: (Floating a, Ord a) => Orbit a -> Time a -> Angle a
meanAnomalyAtTime o t = t |*| n
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
        untypedE = delRad _E
        _M = addRad (untypedE |-| e |*| sin untypedE)

-- | Calculate the mean anomaly, M, of a hyperbolic orbit when at hyperbolic
-- anomaly H
meanAnomalyAtHyperbolicAnomaly
  :: (Floating a, Ord a) => Orbit a -> AngleH a -> Maybe (Angle a)
meanAnomalyAtHyperbolicAnomaly o _H = case classify o of
  Hyperbolic -> Just _M
  _          -> Nothing
 where
  e  = eccentricity o
  _M = addRad $ e * qSinh _H - quantity (_H # RadianHyperbolic)

-- | Calculate the mean anomaly, M, of an orbiting body when at the given true
-- anomaly, ν.
--
-- The number of orbits represented by the anomalies is preserved;
-- i.e. M `div` 2π = ν `div` 2π
--
-- Returns 'Nothing' for parabolic orbits.
--
-- Returns 'Nothing' when the trajectory is not defined for the given true
-- anomaly.
meanAnomalyAtTrueAnomaly :: (Real a, Floating a)
                         => Orbit a -> Angle a -> Maybe (Angle a)
meanAnomalyAtTrueAnomaly o ν = case classify o of
  Parabolic -> Nothing
  Elliptic -> meanAnomalyAtEccentricAnomaly o <=<
              eccentricAnomalyAtTrueAnomaly o $ ν
  Hyperbolic -> meanAnomalyAtHyperbolicAnomaly o <=<
                hyperbolicAnomalyAtTrueAnomaly o $ ν

---------
-- To eccentric
---------

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
  where (n, wrappedM) = second (# [si|rad|]) (_M `divMod'` turn)
        e = eccentricity o # [si||]
        _MFloat = rad . realToFrac $ wrappedM
        oFloat = unsafeMapOrbit realToFrac o
        initialGuessFloat :: Angle Float
        Just initialGuessFloat = eccentricAnomalyAtMeanAnomalyFloat oFloat _MFloat
        initialGuess = realToFrac . (# [si|rad|]) $ initialGuessFloat
        err :: (Mode b, Floating b, Scalar b ~ a) => b -> b
        err _E = auto wrappedM - (_E - auto e * sin _E)
        wrappedE = fmap rad . convergeErr (runId . abs . err .  Id) $
                   findZeroNoEq err initialGuess
        _E = (|+| (unsafeMapUnit fromInteger n |*| turn)) <$> wrappedE

-- | 'eccentricAnomalyAtMeanAnomaly' specialized to 'Float'.
--
-- This function is used to calculate the initial guess for
-- 'eccentricAnomalyAtMeanAnomaly'.
eccentricAnomalyAtMeanAnomalyFloat :: Orbit Float -> Angle Float -> Maybe (Angle Float)
eccentricAnomalyAtMeanAnomalyFloat o _M = case classify o of
                                            Elliptic -> Just _E
                                            _ -> Nothing
  where wrappedM = (_M `mod'` turn) # [si|rad|]
        e = eccentricity o # [si||]
        sinM = sin wrappedM
        cosM = cos wrappedM
        -- Use a better initial guess
        -- http://alpheratz.net/dynamics/twobody/KeplerIterations_summary.pdf
        initialGuess = wrappedM +
                       e * sinM +
                       e * e * sinM * cosM +
                       0.5 * e * e * e * sinM * (3 * cosM * cosM - 1)
        _E :: Angle Float
        _E = rad . last . take 5 $
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
        cosν = cos (ν # [si|rad|])
        -- sinν = sin (wrappedν # [si|rad|])
        e = eccentricity o # [si||]
        wrappedE = rad $ acos ((e + cosν) / (1 + e * cosν))
        -- wrappedE = rad $ atan2 (sqrt (1 - e*e) * sinν) (e + cosν)
        _E = if wrappedν < halfTurn
               then (unsafeMapUnit fromInteger n |*| turn) |+| wrappedE
               else (unsafeMapUnit fromInteger (n+1) |*| turn) |-| wrappedE

---------
-- To hyperbolic
---------

hyperbolicAnomalyAtTime
  :: forall a
   . (Converge [a], RealFloat a)
  => Orbit a
  -> Time a
  -> Maybe (AngleH a)
hyperbolicAnomalyAtTime o =
  hyperbolicAnomalyAtMeanAnomaly o . meanAnomalyAtTime o

hyperbolicAnomalyAtMeanAnomaly
  :: forall a
   . (Converge [a], RealFloat a)
  => Orbit a
  -> Angle a
  -> Maybe (AngleH a)
hyperbolicAnomalyAtMeanAnomaly o _M = case classify o of
  Hyperbolic -> _H
  _          -> Nothing
 where
  e                       = eccentricity o # [si||]
  _M'                     = _M # [si|rad|]
  _MDouble                = realToFrac _M'
  Just initialGuessDouble = hyperbolicAnomalyAtMeanAnomalyDouble
    (unsafeMapOrbit realToFrac o)
    (rad _MDouble)
  initialGuess = realToFrac . (# RadianHyperbolic) $ initialGuessDouble
  err :: (Mode b, Floating b, Scalar b ~ a) => b -> b
  err _H = auto _M' - (auto e * sinh _H - _H)
  _H = fmap rdh . convergeErr (runId . abs . err . Id) $ findZeroNoEq
    err
    initialGuess

-- | Calculate the hyperbolic anomaly, H, at a given mean anomaly. Unline
-- 'eccentricAnomalyAtMeanAnomalyFloat' this uses double precision floats to
-- help avoid overflowing.
hyperbolicAnomalyAtMeanAnomalyDouble
  :: Orbit Double -> Angle Double -> Maybe (AngleH Double)
hyperbolicAnomalyAtMeanAnomalyDouble o _M = case classify o of
  Hyperbolic -> case _H of
    -- If you hit this, a better initial guess would probably help
    Qu x | isNaN x -> error "NaN while trying to find hyperbolic anomaly"
    _              -> Just _H
  _ -> Nothing
 where
  -- Perhaps use something like https://www.researchgate.net/publication/226007277_A_Method_Solving_Kepler%27s_Equation_for_Hyperbolic_Case
  e            = eccentricity o # [si||]
  _M'          = _M # [si|rad|]
  -- TODO: A better guess here
  initialGuess = _M'
  _H           = rdh . last . take 200 $ Newton.findZero
    (\_H -> auto _M' - (auto e * sinh _H - _H))
    initialGuess

-- | Returns the hyperbolic anomaly, H, for an orbit at true anomaly ν.
--
-- Returns 'Nothing' when given an 'Elliptic' or 'Parabolic' orbit, or a true
-- anomaly out of the range of the hyperbolic orbit.
hyperbolicAnomalyAtTrueAnomaly
  :: (Floating a, Ord a) => Orbit a -> Angle a -> Maybe (AngleH a)
hyperbolicAnomalyAtTrueAnomaly o ν = case classify o of
  _ | Just d <- hyperbolicDepartureAngle o, qAbs ν |>| d -> Nothing
  Hyperbolic -> Just _H
  _          -> Nothing
 where
  e     = eccentricity o
  coshH = (qCos ν + e) / (1 + e * qCos ν)
  sign  = signum (ν # [si|rad|])
  _H    = sign *| qArcCosh coshH

---------
-- To true
---------

-- | Calculate the true anomaly, ν, of a body at time since periapse, t.
trueAnomalyAtTime
  :: forall a . (Converge [a], RealFloat a) => Orbit a -> Time a -> Angle a
trueAnomalyAtTime o t = case classify o of
  Elliptic   -> trueAnomalyAtMeanAnomaly o _M
  Hyperbolic -> trueAnomalyAtMeanAnomaly o _M
  Parabolic ->
    let _A = (3 |/| 2) |*| qSqrt (μ |/| (2 |*| qCube (l |/| 2))) |*| t
        _B = qCubeRoot (_A |+| qSqrt (qSq _A |+| 1))
    in  2 |*| qArcTan (_B - recip _B)
 where
  μ  = primaryGravitationalParameter o
  l  = semiLatusRectum o
  _M = meanAnomalyAtTime o t

-- | Calculate the true anomaly, ν, of an orbiting body when it has the given
-- mean anomaly, _M.
trueAnomalyAtMeanAnomaly
  :: (Converge [a], RealFloat a) => Orbit a -> Angle a -> Angle a
trueAnomalyAtMeanAnomaly o _M = case classify o of
  Elliptic -> fromJust
    (trueAnomalyAtEccentricAnomaly o <=< eccentricAnomalyAtMeanAnomaly o $ _M)
  Hyperbolic -> fromJust
    (trueAnomalyAtHyperbolicAnomaly o <=< hyperbolicAnomalyAtMeanAnomaly o $ _M)
  _ -> error "trueAnomalyAtMeanAnomaly is not defined for Parabolic orbits"

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
  where (n, wrappedE) = bimap (unsafeMapUnit fromInteger) (# [si|rad|]) $
                        _E `divMod'` turn
        e = eccentricity o # [si||]
        wrappedν = rad $ 2 * atan2 (sqrt (1 + e) * sin (wrappedE / 2))
                                   (sqrt (1 - e) * cos (wrappedE / 2))
        ν = turn |*| n |+| wrappedν

trueAnomalyAtHyperbolicAnomaly
  :: (Ord a, Floating a) => Orbit a -> AngleH a -> Maybe (Angle a)
trueAnomalyAtHyperbolicAnomaly o _H = case classify o of
  Hyperbolic -> Just ν
  _          -> Nothing
 where
  e         = eccentricity o
  tanνOver2 = sqrt ((e + 1) / (e - 1)) * qTanh (_H |/| 2)
  ν         = 2 |*| qArcTan tanνOver2

----------------------------------------------------------------
-- Other orbital properties
----------------------------------------------------------------

-- | The distance, r, from the primary body to the orbiting body at a particular
-- true anomaly.
radiusAtTrueAnomaly :: (Ord a, Floating a) => Orbit a -> Angle a -> Distance a
radiusAtTrueAnomaly o trueAnomaly = case semiMajorAxis o of
  Just _  -> l |/| (1 |+| e |*| qCos ν)
  Nothing -> (qSq h |/| μ) |*| (1 |/| (1 |+| qCos ν))
 where
  h = specificAngularMomentum o
  e = eccentricity o
  ν = trueAnomaly
  μ = primaryGravitationalParameter o
  l = semiLatusRectum o

-- | What is the speed, v, of a body at a particular true anomaly
speedAtTrueAnomaly :: (Ord a, Floating a) => Orbit a -> Angle a -> Speed a
speedAtTrueAnomaly o trueAnomaly = case semiMajorAxis o of
  Nothing -> qSqrt (μ |*| 2 |/| r)
  Just a  -> qSqrt (μ |*| (2 |/| r |-| 1 |/| a))
 where
  ν = trueAnomaly
  μ = primaryGravitationalParameter o
  r = radiusAtTrueAnomaly o ν

-- | Specific angular momentum, h, is the angular momentum per unit mass
specificAngularMomentum :: Floating a => Orbit a -> Quantity [si|m^2 s^-1|] a
specificAngularMomentum o = qSqrt (μ |*| l)
  where
    μ = primaryGravitationalParameter o
    l = semiLatusRectum o

-- | Specific orbital energy, ε, is the orbital energy per unit mass
specificOrbitalEnergy
  :: (Ord a, Floating a) => Orbit a -> Quantity [si|J / kg|] a
specificOrbitalEnergy o = case semiMajorAxis o of
  Just a  -> qNegate (μ |/| (2 |*| a))
  Nothing -> zero
  where μ = primaryGravitationalParameter o

-- | Specific potential energy, εp, is the potential energy per unit mass at a
-- particular true anomaly
specificPotentialEnergyAtTrueAnomaly
  :: (Ord a, Floating a) => Orbit a -> Angle a -> Quantity [si|J / kg|] a
specificPotentialEnergyAtTrueAnomaly o ν = qNegate (μ |/| r)
 where
  r = radiusAtTrueAnomaly o ν
  μ = primaryGravitationalParameter o

-- | Specific kinetic energy, εk, is the kinetic energy per unit mass at a
-- particular true anomaly
specificKineticEnergyAtTrueAnomaly
  :: (Ord a, Floating a) => Orbit a -> Angle a -> Quantity [si|J / kg|] a
specificKineticEnergyAtTrueAnomaly o ν = qSq (speedAtTrueAnomaly o ν) |/| 2

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | The escape velocity for a primary with specified gravitational parameter
-- at a particular distance.
escapeVelocityAtDistance
  :: (Floating a) => Quantity [si| m^3 s^-2 |] a -> Distance a -> Speed a
escapeVelocityAtDistance μ r = qSqrt (2 |*| μ |/| r)
