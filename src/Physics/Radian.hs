{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- | This module just exports instances for the "rad" unit.
module Physics.Radian
  ( turn
  , halfTurn
  , canonicalizeAngle
  ) where

import Data.UnitsOfMeasure.Extra

-- | One complete revolution in radians
turn :: Floating a => Quantity a [u|rad|]
turn = 2 *: halfTurn

halfTurn :: Floating a => Quantity a [u|rad|]
halfTurn = [u|rad|] pi

-- | "modulate" the angle into the range [-π..π)
-- This should not be used on the mean anomaly for parabolic or hyperbolic
-- orbits, as the mean anomaly is unbounded.
canonicalizeAngle :: (Real a, Floating a)
                  => Quantity a [u|rad|] -> Quantity a [u|rad|]
canonicalizeAngle = (-: halfTurn) . snd . (`divMod'` turn) . (+: halfTurn)
