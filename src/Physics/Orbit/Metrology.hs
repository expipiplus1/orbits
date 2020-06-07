{-# language QuasiQuotes #-}

module Physics.Orbit.Metrology where

import           Data.Metrology
import           Data.Metrology.TH
import           Data.Units.SI.Parser

declareDimension "PlaneAngleHyperbolic"
declareCanonicalUnit "RadianHyperbolic" [t| PlaneAngleHyperbolic |] (Just "rdh")
type instance DefaultUnitOfDim PlaneAngleHyperbolic = RadianHyperbolic

type Quantity u = MkQu_ULN u 'DefaultLCSU
-- | A measure in seconds.
type Time     = Quantity [si|s|]
-- | A measure in meters.
type Distance = Quantity [si| m |]
-- | A measure in meters per second.
type Speed    = Quantity [si| m s^-1 |]
-- | A measure in kilograms.
type Mass     = Quantity [si| kg |]
-- | A measure in radians.
type Angle    = Quantity [si| rad |]
-- | A measure in radians (hyperbolic)
type AngleH   = Quantity RadianHyperbolic
-- | A unitless measure.
type Unitless = Quantity [si||]
