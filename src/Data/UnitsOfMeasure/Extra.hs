{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Data.UnitsOfMeasure.Extra
  ( module Data.UnitsOfMeasure
  , cube
  , square
  ) where

import Data.UnitsOfMeasure

cube :: Num a => Quantity a v -> Quantity a (v ^: 3)
cube x = x *: x *: x

square :: Num a => Quantity a v -> Quantity a (v ^: 2)
square x = x *: x
