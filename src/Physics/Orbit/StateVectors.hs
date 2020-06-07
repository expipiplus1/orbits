{-# language QuasiQuotes #-}

module Physics.Orbit.StateVectors
  ( -- *** Types
    StateVectors(..)
  , Position
  , Velocity
    -- *** Conversion to state vectors
  , stateVectorsAtTrueAnomaly
  , positionAtTrueAnomaly
  , positionInPlaneAtTrueAnomaly
  , velocityAtTrueAnomaly
  , velocityInPlaneAtTrueAnomaly
    -- *** Conversion from state vectors
  , elementsFromStateVectors
  , eccentricityVector
  , trueAnomalyAtPosition
    -- *** Rotations to and from orbital plane
  , orbitalPlaneQuaternion
  , rotateToPlane
  , rotateFromPlane
    -- *** other utilities
  , flightPathAngleAtTrueAnomaly
  , specificAngularMomentumVector
  ) where

import           Control.Lens.Operators         ( (^.) )
import           Data.Coerce
import           Data.Constants.Mechanics.Extra
import           Data.Metrology
import           Data.Metrology.Extra
import           Data.Metrology.Unsafe          ( Qu(..) )
import           Data.Units.SI.Parser
import           Linear.Conjugate
import           Linear.Quaternion
import           Linear.V3
import           Physics.Orbit

type Position a = V3 (Distance a)
type Velocity a = V3 (Speed a)

data StateVectors a = StateVectors
  { position :: Position a
  , velocity :: Velocity a
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- Conversiont to state vectors
----------------------------------------------------------------

-- | Get the position in space of a body after rotating it according to the
-- inclination and periapsis specifier.
positionAtTrueAnomaly
  :: (Conjugate a, RealFloat a) => Orbit a -> Angle a -> Position a
positionAtTrueAnomaly o = rotateFromPlane o . positionInPlaneAtTrueAnomaly o

-- | Get the position of a body relative to the orbital plane
positionInPlaneAtTrueAnomaly
  :: (Ord a, Floating a) => Orbit a -> Angle a -> Position a
positionInPlaneAtTrueAnomaly o ν = r
 where
  radius = radiusAtTrueAnomaly o ν
  r      = V3 (qCos ν |*| radius) (qSin ν |*| radius) zero

-- | Get the velocity in space of a body after rotating it according to the
-- inclination and periapsis specifier.
velocityAtTrueAnomaly
  :: (Conjugate a, RealFloat a) => Orbit a -> Angle a -> Velocity a
velocityAtTrueAnomaly o = rotateFromPlane o . velocityInPlaneAtTrueAnomaly o

-- | The in-plane velocity of a body
velocityInPlaneAtTrueAnomaly
  :: (Ord a, Floating a) => Orbit a -> Angle a -> Velocity a
velocityInPlaneAtTrueAnomaly o ν = v
 where
  μ    = primaryGravitationalParameter o
  e    = eccentricity o
  r    = radiusAtTrueAnomaly o ν
  h    = specificAngularMomentum o
  cosν = qCos ν
  sinν = qSin ν
  vr   = μ |*| e |*| sinν |/| h
  vtA  = h |/| r
  v    = V3 (vr |*| cosν |-| vtA |*| sinν) (vr |*| sinν |+| vtA |*| cosν) zero

stateVectorsAtTrueAnomaly
  :: (Conjugate a, RealFloat a) => Orbit a -> Angle a -> StateVectors a
stateVectorsAtTrueAnomaly o ν = StateVectors r v
 where
  r = positionAtTrueAnomaly o ν
  v = velocityAtTrueAnomaly o ν

----------------------------------------------------------------
-- Conversion from state vectors
----------------------------------------------------------------

-- Thanks to https://downloads.rene-schwarz.com/download/M002-Cartesian_State_Vectors_to_Keplerian_Orbit_Elements.pdf
elementsFromStateVectors
  :: (Ord a, Floating a, Conjugate a, RealFloat a, Show a)
  => Quantity [si| m^3 s^-2 |] a
  -> StateVectors a
  -> (Orbit a, Angle a)
elementsFromStateVectors μ sv@(StateVectors r v) = (o, ν)
 where
  o     = Orbit e q inclinationSpecifier' periapsisSpecifier' μ

  h     = specificAngularMomentumVector sv
  n     = V3 (qNegate (h ^. _y)) (h ^. _x) zero

  e'    = eccentricityVector μ sv
  e     = qNorm e'
  eNorm = (recip e *) <$> e'

  aInv  = (2 |/| qNorm r) |-| (qQuadrance v |/| μ)
  a     = qRecip aInv
  q     = if aInv == zero -- parabolic trajectory
    then qQuadrance h |/| (2 |*| μ)
    else a |*| (1 - e)

  ν = if e == zero
    then -- fall back to the slower version if this is a circular orbit
         trueAnomalyAtPosition o r
    else
      let cosν = eNorm `qDot` qNormalize r
      in  if r `qDot` v >= zero then qArcCos cosν else turn |-| qArcCos cosν

  inclinationSpecifier' =
    let i    = qArcCos ((h ^. _z) |/| qNorm h)
        cosΩ = n ^. _x |/| qNorm n
        _Ω   = if n ^. _y >= zero then qArcCos cosΩ else turn |-| qArcCos cosΩ
    in  if h ^. _x == zero && h ^. _y == zero
          then NonInclined
          else Inclined _Ω i

  -- If the orbit is not inclined, ω is relative to the reference direction
  -- [1,0,0]
  periapsisSpecifier' =
    let cosω = case inclinationSpecifier' of
          Inclined _ _ -> qNormalize n `qDot` eNorm
          NonInclined  -> eNorm ^. _x
        -- ω = if (e' ^. _z) >= zero then qArcCos cosω else turn |-| qArcCos cosω
        ω = case inclinationSpecifier' of
          Inclined _ _ ->
            if (e' ^. _z) >= zero then qArcCos cosω else turn |-| qArcCos cosω
          NonInclined ->
            let sinω = eNorm ^. _y in qArcTan2 sinω cosω `mod'` turn
    in  if e == zero then Circular else Eccentric ω

-- | Calculate the true anomaly, ν, of a body at position, r, given its orbital
-- elements.
trueAnomalyAtPosition
  :: (Conjugate a, RealFloat a) => Orbit a -> Position a -> Angle a
trueAnomalyAtPosition o r = ν
 where
  V3 (Qu x) (Qu y) _ = rotateToPlane o r
  ν                  = atan2 y x % [si|rad|]

-- | Calculate the momentum vector, h, given state vectors
specificAngularMomentumVector
  :: Num a => StateVectors a -> V3 (Quantity [si|m^2 / s|] a)
specificAngularMomentumVector (StateVectors r v) = r `qCross` v

-- | Calculate the eccentricity vector, e, given state vectors
eccentricityVector
  :: Floating a
  => Quantity [si| m^3 s^-2 |] a
  -> StateVectors a
  -> V3 (Unitless a)
eccentricityVector μ sv@(StateVectors r v) = e
 where
  e = (v `qCross` h) |^/| μ |^-^| qNormalize r
  h = specificAngularMomentumVector sv

----------------------------------------------------------------
-- Rotations to and from the orbital plane
----------------------------------------------------------------

-- | Rotate a position relative to the orbital plane according to the
-- inclination specifier and periapsis specifier.
--
-- The orbital plane is perpendicular to the z axis
rotateFromPlane
  :: (Conjugate a, RealFloat a)
  => Orbit a
  -> V3 (Qu u l a)
  -> V3 (Qu u l a)
rotateFromPlane = qRotate . orbitalPlaneQuaternion

-- | Rotate a position such that is is relative to the orbital plane according
-- to the inclination specifier and periapsis specifier.
--
-- The orbital plane is perpendicular to the z axis
rotateToPlane
  :: (Conjugate a, RealFloat a) => Orbit a -> V3 (Qu u l a) -> V3 (Qu u l a)
rotateToPlane = qRotate . conjugate . orbitalPlaneQuaternion

-- | A quaternion representing the rotation of the orbital plane
orbitalPlaneQuaternion :: RealFloat a => Orbit a -> Quaternion a
orbitalPlaneQuaternion o = lon * per
 where
  per = case periapsisSpecifier o of
    Eccentric ω -> rotateZ ω
    Circular    -> 1
  lon = case inclinationSpecifier o of
    Inclined _Ω i -> rotateZ _Ω * rotateX i
    NonInclined   -> 1

----------------------------------------------------------------
-- Orbit Utils
----------------------------------------------------------------

-- | Get the flight path angle, φ, of a body a a specific true anomaly. This is
-- the angle of the body's motion relative to a vector perpendicular to the
-- radius.
flightPathAngleAtTrueAnomaly
  :: (Real a, Floating a) => Orbit a -> Angle a -> Angle a
flightPathAngleAtTrueAnomaly o ν = sign (qArcCos cosφ)
 where
  cosφ = h |/| (r |*| v)
  sign = if (ν `mod'` turn) < halfTurn then id else qNegate
  r    = radiusAtTrueAnomaly o ν
  v    = speedAtTrueAnomaly o ν
  h    = specificAngularMomentum o

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

qRotate
  :: forall a q
   . (Coercible (q a) a, Conjugate a, RealFloat a)
  => Quaternion a
  -> V3 (q a)
  -> V3 (q a)
qRotate = coerce (rotate @a)

rotateX :: Floating a => Angle a -> Quaternion a
rotateX θ = Quaternion (cos half) (V3 (sin half) 0 0)
  where half = (θ # [si|rad|]) / 2

_rotateY :: Floating a => Angle a -> Quaternion a
_rotateY θ = Quaternion (cos half) (V3 0 (sin half) 0)
  where half = (θ # [si|rad|]) / 2

rotateZ :: Floating a => Angle a -> Quaternion a
rotateZ θ = Quaternion (cos half) (V3 0 0 (sin half))
  where half = (θ # [si|rad|]) / 2

{-


orbitalPlaneQuaternion :: Orbit -> Quaternion Double
orbitalPlaneQuaternion Elliptic{..} = l * p
  where p = case periapsisSpecifier of
              Eccentric ω -> rotateZ ω
              Circular -> noRotation
        l = case longitudeSpecifier of
              Inclined{..} -> rotateZ longitudeOfAscendingNode * rotateX inclination
              NonInclined -> noRotation

rotateToWorld :: Orbit -> V3 Double -> V3 Double
rotateToWorld orbit = rotate (orbitalPlaneQuaternion orbit)

rotateToPlane :: Orbit -> V3 Double -> V3 Double
rotateToPlane orbit = rotate (conjugate (orbitalPlaneQuaternion orbit))

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

-- also equal to sqrt(μ/a^3)
averageAngularVelocity :: Orbit -> Angle
averageAngularVelocity orbit = 2 * pi / p
  where p = period orbit

distance :: Orbit -> Angle -> Distance
distance orbit@Elliptic{..} trueAnomaly = semilatusRectum orbit / (1 + eccentricity * cos trueAnomaly)


{-
eccentricAnomaly :: Orbit -> Angle -> Angle
eccentricAnomaly Elliptic{..} trueAnomaly = acos ((eccentricity + cosTrue)/(1 + eccentricity * cosTrue))
  where cosTrue = cos trueAnomaly

meanAnomaly :: Orbit -> Angle -> Angle
meanAnomaly orbit@Elliptic{..} trueAnomaly = e - eccentricity * sin e
  where e = eccentricAnomaly orbit trueAnomaly
  -}

eccentricityVector :: Orbit -> Angle -> V3 Double
eccentricityVector orbit trueAnomaly = eccentricityVectorFromState μ sv
  where sv = stateVectorsFromOrbit orbit trueAnomaly
        μ = primaryGravitationalParameter orbit

eccentricityVectorFromState :: Double -> StateVectors -> V3 Double
eccentricityVectorFromState primaryGravitationalParameter StateVectors{..} = (v `cross` h) ^/ μ - normalize r
  where μ = primaryGravitationalParameter
        r = position
        v = velocity
        h = r `cross` v

trueAnomalyFromState :: Orbit -> StateVectors -> Angle
trueAnomalyFromState orbit stateVectors = if r `dot` v >= 0 then ν else 2 * pi - ν
  where e = eccentricityVectorFromState μ stateVectors
        r = position stateVectors
        v = velocity stateVectors
        ν = acos $ (e `dot` r) / (norm e * norm r)
        μ = primaryGravitationalParameter orbit

orbitalSpeed :: Orbit -> Angle -> Double
orbitalSpeed orbit trueAnomaly = v
  where d = Orbit.distance orbit trueAnomaly
        --ν = trueAnomaly
        μ = primaryGravitationalParameter orbit
        a = semiMajorAxis orbit
        v = if | isElliptic orbit ||
                 isHyperbolic orbit -> sqrt (μ * (2 / d - 1 / a))
               | isParabolic orbit -> sqrt (μ * 2 / d)

velocityAngleFromPrograde :: Orbit -> Angle -> Angle
velocityAngleFromPrograde orbit trueAnomaly = φ
  where ν = trueAnomaly
        e = eccentricity orbit
        φ = if | isElliptic orbit ||
                 isHyperbolic orbit -> atan2 (e * sin ν) (1 + e * cos ν)
               | isParabolic orbit -> ν / 2

-- | Calculate the state vectors relative to the orbital plane
--
-- The Z dimension is perpendicular to the orbital plane and hence is
-- always zero
orbitalPlaneStateVectors :: Orbit -> Angle -> StateVectors
orbitalPlaneStateVectors orbit trueAnomaly = StateVectors r v
  where d = Orbit.distance orbit trueAnomaly
        ν = trueAnomaly
        r = V3 (d * cos ν) (d * sin ν) 0
        e = eccentricity orbit
        --a = semiMajorAxis orbit
        u = V3 (1 + e * cos ν) (e * sin ν) 0
        v = u ^* orbitalSpeed orbit trueAnomaly
        --n = averageAngularVelocity orbit
        --v = V3 (- sin ν) (e + cos ν) 0 ^* (n * a / sqrt (1 - e^2))

rotateX :: Angle -> Quaternion Double
rotateX = axisAngle $ V3 1 0 0

rotateY :: Angle -> Quaternion Double
rotateY = axisAngle $ V3 0 1 0

rotateZ :: Angle -> Quaternion Double
rotateZ = axisAngle $ V3 0 0 1

noRotation :: Num a => Quaternion a
noRotation = Quaternion 1 (V3 0 0 0)

stateVectorsFromOrbit :: Orbit -> Angle -> StateVectors
stateVectorsFromOrbit orbit trueAnomaly = StateVectors r v
  where ν = trueAnomaly
        r = positionAtTrueAnomaly orbit ν
        v = velocityAtTrueAnomaly orbit ν
        {-
        o = orbitalPlaneStateVectors orbit trueAnomaly
        r' = position o
        v' = velocity o
        p = case periapsisSpecifier of
              Eccentric ω -> rotateZ ω
              Circular -> noRotation
        l = case longitudeSpecifier of
              Inclined{..} -> rotateZ longitudeOfAscendingNode * rotateX inclination
              NonInclined -> noRotation
        r = (l * p) `rotate` r'
        v = (l * p) `rotate` v'
        -}

orbitFromStateVectors :: StateVectors -> Double -> (Orbit, Angle)
orbitFromStateVectors sv@StateVectors{..} primaryGravitationalParameter = (orbit, ν)
  where r = position
        v = velocity
        μ = primaryGravitationalParameter
        -- `h` is the specific relative angular momentum
        h@(V3 _ _ hz) = r `cross` v
        -- `an` is the vector pointing towards the ascending node
        -- Todo, handle inclinations of 90 degrees here
        an@(V3 anx any _) = let an' = V3 0 0 1 `cross` h
                            in if nearZero (norm an') then V3 1 0 0 else an'
        -- `ev` is the eccentricity vector
        ev@(V3 evx evy evz) = eccentricityVectorFromState μ sv
        -- ε is the specific orbital energY
        ε = quadrance v / 2 - μ / norm r
        -- `a` is the semimajor axis
        --a = μ * norm r / (2 * μ - norm r * quadrance v)
        a = let a' = μ / (2 * ε)
            in if | isElliptic orbit ||
                    isHyperbolic orbit -> -a'
                  | isParabolic orbit -> error "parabolic orbits don't have a well defined semi-major axis"
        -- `e` is the eccentricity, Sometimes these numbers come out a tiny
        -- bit negative so clamp with 0
        --e = sqrt (max 0 $ 1 - quadrance h / (μ * a))
        e = norm ev
        -- `i` is the inclination
        i = acos $ hz / norm h
        -- `lan` is the longitude of the ascending node, sometimes known as Ω
        lan = let lan' = acos (anx / norm an)
              in if any >= 0 then lan' else 2 * pi - lan'
        -- `ω` is the argument of periapsis
        ω = let ω' = acos ((an `dot` ev)/(norm an * norm ev))
            in if evz < 0 then 2 * pi - ω' else ω'
        --ω = let ω' = atan2 evy evx
            --in if (r `cross` v < 0) then 2 * pi - ω' else ω'
        -- `ν` is the true anomaly
        ν = let ν' = acos ((ev `dot` r)/(norm ev * norm r))
            in if | isElliptic orbit -> if r `dot` v < 0 then 2 * pi - ν' else ν'
                  | isHyperbolic orbit -> if r `dot` v < 0 then -ν' else ν'
        orbit = Elliptic{ eccentricity = e
               , semiMajorAxis = a
               , longitudeSpecifier = if nearZero i then NonInclined
                                                    else Inclined { inclination = i
                                                                  , longitudeOfAscendingNode = lan
                                                                  }
               , periapsisSpecifier = if nearZero e then Circular
                                                    else Eccentric{argumentOfPeriapsis = ω}
               , primaryGravitationalParameter = μ}

lambert :: V3 Double -> V3 Double -> Double -> Double -> [(V3 Double, V3 Double)]
lambert r1 r2 primaryGravitationalParameter transferTime = [(v1, v2)]
  where μ = primaryGravitationalParameter
        h = r1 `cross` r2
        cosθ = (r1 `dot` r2) / (norm r1 * norm r2)
        θ = let θ' = acos cosθ
            in if | (h^._z) >= 0 -> θ' -- Todo, fixme
                  | otherwise -> 2 * π - θ'
        d = if | 0 <= θ && θ <= π -> 1
               | π < θ && θ <= 2 * π -> -1
        τ = d * sqrt (norm r1 * norm r2 * (1 + cosθ)) / (norm r1 + norm r2)
        s = sqrt $ ((norm r1 + norm r2)^3) / μ
        n = 0
        wse k = let v = k - sqrt 2
                in sqrt 2/3 - v/5 + 2/35*sqrt 2*v^2 - 2/63*v^3 + 2/231*sqrt 2*v^4 -
                   2/429*v^5 + 8/6435*sqrt 2*v^6 - 8/12155*v^7 + 8/46189*sqrt 2*v^8 -
                   8/88179*v^9 + 16/676039*sqrt 2*v^10 - 16/1300075*v^11 +
                   16/5014575*sqrt 2*v^12 - 16/9694845*v^13 +
                   128/300540195*sqrt 2*v^14 - 128/583401555*v^15 +
                   128/2268783825*sqrt 2*v^16
        tof n k = (tofk, tof'k, tof''k)
                  where tofk = s * sqrt (1 - k * τ) * (τ + (1 - k * τ) * w) -- 26
                        tof'k = -tofk / (2 * c) + s * τ * sqrt (c * τ) * (w' * c - w)
                        tof''k = -tofk / (4 * c^2) + s * τ * sqrt (c * τ) * (w / c + c * w'' - 3 * w')
                        c = (1 - k * τ) / τ
                        ε = 2e-2
                        w = if | k < sqrt 2 - ε ->
                                  ((1 - signum k) * π + signum k * acos (1 - m) + 2 * π * n) /
                                  sqrt (m^3) -
                                  k/m
                               | k > sqrt 2 + ε -> - acosh (1 - m) / sqrt (-m^3) - k / m
                               | otherwise -> ws -- 27
                        w' = if | k < sqrt 2 - ε -> (-2 + 3 * w * k) / m
                                | k > sqrt 2 + ε -> (-2 + 3 * w * k) / (-m)
                                | otherwise -> ws'
                        w'' = if | k < sqrt 2 - ε -> (5 * w' * k + 3 * w) / m
                                 | k > sqrt 2 + ε -> (5 * w' * k + 3 * w) / (-m)
                                 | otherwise -> ws''
                        (ws:ws':ws'':_) = diffs wse k
                        m = 2 - k^2
        --Right k = traceShowId $ newton (\k -> let (a, b, _) = tof (traceShowId k) in (a - transferTime, b)) (-sqrt 2) (sqrt 2) 1e-6
        initialGuess = 0
        --isValid = (&&) <$> (not . isNaN) <*> (-sqrt 2<)
        --ks = filter (isValid . snd) . zip initialGuesses $ halley (\k -> let (a,b,c) = tof n k in (a - transferTime, b, c)) <$> initialGuesses
        --Right k = newton (\k -> let (a,b,_) = tof n k in (a - transferTime, b)) (-sqrt 2) (sqrt 2) 1e-6
        --k = -1.414284878632464
        --k = snd . head $ ks
        k = halley (\k -> let (a,b,c) = tof n k in (a - transferTime, b, c)) initialGuess
        --kMinTime n = (\(Right y) -> y) $ newton (\k -> let (_, y',y'') = tof n k in (y', y'')) (-1) 1 1e-6
        --kbs = kMinTime <$> [1..]
        --tbs = (^._1) . uncurry tof <$> zip [1..] kbs
        f = 1 - (1 - k * τ) * (norm r1 + norm r2) / norm r1 -- 1 - (1 - k * τ) / norm r1
        --g' = 1 - (1 - k * τ) / norm r2
        g' = 1 - (1 - k * τ) * (norm r1 + norm r2) / norm r2
        --g = s * τ * sqrt ((1 - k * τ) * μ) -- τ * (norm r1 + norm r2) * sqrt (1 - k * τ)
        g = s * τ * sqrt (1 - k * τ)
        v1 = (r2 - f *^ r1) ^/ g -- ^* sqrt μ
        v2 = (g' *^ r2 - r1) ^/ g
        {-debugInfo = "kbs: " ++ show (take 5 kbs) ++
                    "\ntbs: " ++ show (take 5 tbs) ++
                    -- "\nks: " ++ show ks ++
                    "\nd: " ++ show d ++
                    "\nτ (tau): " ++ show τ ++
                    "\nθ (theta): " ++ show θ ++
                    "\nk: " ++ show k ++
                    "\nn: " ++ show n ++
                    "\ntof: " ++ show (tof n k) ++
                    "\nf: " ++ show f ++
                    "\ng: " ++ show g ++
                    "\nr1: " ++ show r1 ++
                    "\nr2: " ++ show r2 ++
                    "\nv1: " ++ show v1 ++
                    "\nv2: " ++ show v2 ++
                    "\norbits: "-}
        (orbit1, ν1) = traceShowId $ Debug.Trace.trace debugInfo $ orbitFromStateVectors (StateVectors r1 v1) μ
        (orbit2, ν2) = traceShowId $ orbitFromStateVectors (StateVectors r2 v2) μ
        ma1 = meanAnomalyAtTrueAnomaly orbit1 ν1
        ma2 = meanAnomalyAtTrueAnomaly orbit2 ν2

isValid :: V3 Double -> Bool
isValid = noneOf each isNaN

ballisticTransfer :: (Double -> StateVectors) -> (Double -> StateVectors) -> Double -> Double -> Double -> Double -> (Burn, Burn)
ballisticTransfer fo1 fo2 primaryGravitationalParameter departureMin departureMax maxTransferTime = (b1, b2)
  where (b1, b2, _) = minimumBy (compare `on` (^._3)) ts
        ts = do let numDepartureSamples = 100
                    numArrivalSamples = 100
                    departureInterval = departureMax - departureMin
                d <- [0..numDepartureSamples-1]
                a <- [0..numArrivalSamples-1]
                let departureTime = departureMin + departureInterval * d / (numDepartureSamples - 1)
                    transferTime = maxTransferTime * a / (numArrivalSamples - 1)
                    arrivalTime = departureTime + transferTime
                    StateVectors{position = r1, velocity = v1} = fo1 departureTime
                    StateVectors{position = r2, velocity = v2} = fo2 arrivalTime
                (v1', v2') <- lambert r1 r2 primaryGravitationalParameter transferTime
                guard $ noneOf each isNaN v1'
                guard $ noneOf each isNaN v2'
                let b1 = v1' - v1
                    b2 = v2' - v2
                    δv1 = norm b1
                    δv2 = norm b2
                    δv = δv1 + δv2
                pure (Burn departureTime b1, Burn arrivalTime b2, δv)

toManoeuvreReferenceFrame :: Orbit -> Angle -> V3 Double -> V3 Double
toManoeuvreReferenceFrame orbit trueAnomaly = (m !*)
  where ν = trueAnomaly
        r = normalize $ positionAtTrueAnomaly orbit ν
        v = normalize $ velocityAtTrueAnomaly orbit ν
        prograde = normalize $ v
        normal = normalize $ prograde `cross` (-r)
        radial = normalize $ prograde `cross` normal
        m = V3 prograde normal radial

-}
