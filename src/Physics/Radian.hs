{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- | This module just exports instances for the "rad" unit.
module Physics.Radian
  ( turn
  ) where

import Data.UnitsOfMeasure

[u| rad |]

-- | One complete revolution in radians
turn :: Floating a => Quantity a [u|rad|]
turn = [u|rad|] (2 * pi)
