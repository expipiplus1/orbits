{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UnitsOfMeasure.Extra
  ( module Data.UnitsOfMeasure
  , cube
  , square
  , mod'
  , div'
  , divMod'
  , unsafeMapUnit
  ) where

import           Data.Coerce                  (coerce)
import qualified Data.Fixed                   as F (div', divMod', mod')
import           Data.UnitsOfMeasure
import           Data.UnitsOfMeasure.Internal (Quantity (..))

cube :: Num a => Quantity a v -> Quantity a (v ^: 3)
cube x = x *: x *: x

square :: Num a => Quantity a v -> Quantity a (v ^: 2)
square x = x *: x

mod' :: forall a v. Real a => Quantity a v -> Quantity a v -> Quantity a v
mod' = coerce (F.mod' :: a -> a -> a)

div' :: forall a b u v. (Real a, Integral b)
     => Quantity a u -> Quantity a v -> Quantity b (u /: v)
div' = coerce (F.div' :: a -> a -> b)

divMod' :: forall a b v. (Real a, Integral b)
        => Quantity a v -> Quantity a v
        -> (Quantity b One, Quantity a v)
divMod' = coerce (F.divMod' :: a -> a -> (b, a))

unsafeMapUnit :: (a -> b) -> Quantity a u -> Quantity b u
unsafeMapUnit f (MkQuantity x) = MkQuantity (f x)
