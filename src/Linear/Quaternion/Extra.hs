{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}

module Linear.Quaternion.Extra 
  ( module Linear.Quaternion
  , Angle
  , rotateX
  , rotateY
  , rotateZ
  , noRotation
  ) where

import Linear.Quaternion
import Linear.V3
import Linear.Vector
import Data.UnitsOfMeasure

-- | A measure in radians.
type Angle a = Quantity a [u| rad |]

rotateX :: Floating a => Angle a -> Quaternion a
rotateX = axisAngleNorm (V3 1 0 0) . unQuantity

rotateY :: Floating a => Angle a -> Quaternion a
rotateY = axisAngleNorm (V3 0 1 0) . unQuantity

rotateZ :: Floating a => Angle a -> Quaternion a
rotateZ = axisAngleNorm (V3 0 0 1) . unQuantity

noRotation :: Num a => Quaternion a
noRotation = Quaternion 1 (V3 0 0 0)

-- | @'axisAngle' axis theta@ builds a 'Quaternion' representing a
-- rotation of @theta@ radians about @axis@. @axis@ must be normalized.
axisAngleNorm :: Floating a => V3 a -> a -> Quaternion a
axisAngleNorm axis theta = Quaternion (cos half) (sin half *^ axis)
  where half = theta / 2
{-# INLINE axisAngleNorm #-}

