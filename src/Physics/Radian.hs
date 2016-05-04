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
  ) where

import Data.UnitsOfMeasure

-- | One complete revolution in radians
turn :: Floating a => Quantity a [u|rad|]
turn = 2 *: halfTurn

halfTurn :: Floating a => Quantity a [u|rad|]
halfTurn = [u|rad|] pi
