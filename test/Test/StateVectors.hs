module Test.StateVectors where

import           Control.Lens.Operators         ( (^.) )
import           Data.CReal                     ( CReal )
import           Data.CReal.QuickCheck          ( )
import           Data.Metrology
import           Data.Metrology.Extra
import           Data.Metrology.QuickCheck
import           Data.Units.SI.Parser
import           Linear.Metric
import           Linear.QuickCheck              ( )
import           Linear.V3
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Extra
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH                  ( testGroupGenerator )

import           Physics.Orbit
import           Physics.Orbit.QuickCheck
import           Physics.Orbit.StateVectors

import           Debug.Trace

-- | The type used for tests which require exact arithmetic. They are compared
-- at a resolution of 2^16
type Exact = CReal 16

test_planeRotation :: [TestTree]
test_planeRotation =
  [ testProperty
      "plane rotation inverse"
      (\o -> inverse @(Position Exact) (rotateToPlane o) (rotateFromPlane o))
  ]

test_stateVectorInverse :: [TestTree]
test_stateVectorInverse =
  [ testProperty
    "state vector elements inverse"
    (\(PositiveQuantity μ) sv ->
      let (o, ν) = elementsFromStateVectors @Exact μ sv
          sv'    = stateVectorsAtTrueAnomaly o ν
      in  sv' === sv
    )
  , slowTest $ testProperty
    "elements state vector inverse"
    (\(CanonicalOrbit o) (PositiveQuantity ν) ->
      let μ           = primaryGravitationalParameter @Exact (traceShowId o)
          sv          = stateVectorsAtTrueAnomaly o ν
          (o', ν')    = elementsFromStateVectors μ sv
          isEccentric = case periapsisSpecifier o of
            Circular -> False
            _        -> eccentricity o /= zero
          isPositivelyInclined = case inclinationSpecifier o of
            NonInclined  -> False
            Inclined _ i -> i > zero
          isNotParabolic = case Physics.Orbit.classify o of
            Parabolic -> False
            _         -> True
      in  isEccentric
            &&  isPositivelyInclined
            &&  isNotParabolic
            ==> (o', ν')
            === (o , ν)
    )
  ]

test_positionVelocity :: [TestTree]
test_positionVelocity =
  [ testProperty
    "position magnitude"
    (\o ν ->
      let r1 = fmap (# [si|m|]) . positionAtTrueAnomaly @Exact o $ ν
          r2 = (# [si|m|]) . radiusAtTrueAnomaly o $ ν
      in  r2 * r2 === quadrance r1
    )
  , testProperty
    "position in plane z"
    (\o ν ->
      let r = positionInPlaneAtTrueAnomaly @Float o ν in r ^. _z === zero
    )
  , testProperty
    "velocity magnitude"
    (\o ν ->
      let r1 = fmap (# [si|m/s|]) . velocityAtTrueAnomaly @Exact o $ ν
          r2 = (# [si|m/s|]) . speedAtTrueAnomaly o $ ν
      in  r2 * r2 === quadrance r1
    )
  , testProperty
    "velocity in plane z"
    (\o ν ->
      let v = velocityInPlaneAtTrueAnomaly @Float o ν in v ^. _z === zero
    )
  , testProperty
    "velocity at ν=0"
    (\o ->
      let v     = velocityInPlaneAtTrueAnomaly @Exact o zero
          speed = speedAtTrueAnomaly o zero
      in  v === (V3 zero speed zero)
    )
  , testProperty
    "velocity in circular orbit"
    (\(CircularOrbit o) ν ->
      let v     = velocityInPlaneAtTrueAnomaly @Exact o ν
          speed = speedAtTrueAnomaly o zero
      in  qNorm v === speed
    )
  , testProperty
    "velocity perpendicular to radius in circular orbit"
    (\(CircularOrbit o) ν ->
      let v = velocityInPlaneAtTrueAnomaly @Exact o ν
          r = positionInPlaneAtTrueAnomaly o ν
      in  v `qDot` r === zero
    )
  ]

test_flightPathAngle :: [TestTree]
test_flightPathAngle =
  [ testProperty
    "fpa circular orbit "
    (\(CircularOrbit o) ν ->
      let φ = flightPathAngleAtTrueAnomaly @Exact o ν in φ === zero
    )
  , testProperty
    "fpa angular momentum"
    (\o ν ->
      let φ = flightPathAngleAtTrueAnomaly @Exact o ν
          h = specificAngularMomentum o
          r = radiusAtTrueAnomaly o ν
          v = speedAtTrueAnomaly o ν
      in  h === r |*| v |*| qCos φ
    )
  , testProperty
    "fpa velocity direction"
    (\o ν ->
      let φ = flightPathAngleAtTrueAnomaly @Exact o ν
          r = (# [si|m|]) <$> positionInPlaneAtTrueAnomaly o ν
          v = (# [si|m/s|]) <$> velocityInPlaneAtTrueAnomaly o ν
      in  validTrueAnomaly o ν
            ==>   sin (φ # [si|rad|])
            ===   normalize r
            `dot` normalize v
    )
  ]

test_specificAngularMomentum :: [TestTree]
test_specificAngularMomentum =
  [ testProperty
    "momentum from vectors"
    (\o -> specificAngularMomentum @Exact o === specificAngularMomentumSV o)
  , testProperty
    "momentum vector length"
    (\o ν ->
      let sv = stateVectorsAtTrueAnomaly @Exact o ν
          h1 = specificAngularMomentumVector sv
          h2 = specificAngularMomentum o
      in  qNorm h1 === h2
    )
  ]

prop_specificAngularMomentum :: Orbit Exact -> Property
prop_specificAngularMomentum o =
  specificAngularMomentum o === specificAngularMomentumSV o

specificAngularMomentumSV
  :: (Ord a, Floating a) => Orbit a -> Quantity [si|m^2 s^-1|] a
specificAngularMomentumSV o = rx |*| vy |-| ry |*| vx
   where
    ν          = zero
    V3 rx ry _ = positionInPlaneAtTrueAnomaly o ν
    V3 vx vy _ = velocityInPlaneAtTrueAnomaly o ν

tests :: TestTree
tests = $(testGroupGenerator)

----------------------------------------------------------------
-- Orbit utils
----------------------------------------------------------------

validTrueAnomaly :: (Floating a, Ord a) => Orbit a -> Angle a -> Bool
validTrueAnomaly o ν = case hyperbolicDepartureAngle o of
  Nothing -> True
  Just d  -> qAbs ν < d
