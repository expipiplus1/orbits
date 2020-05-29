{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}

module WrappedAngle
  ( WrappedAngle(..)
  ) where

import           Data.Constants.Mechanics.Extra
import           Data.Metrology
import           Data.Metrology.Extra
import           Data.Metrology.QuickCheck      ( )
import           Data.Metrology.Show            ( )
import           Data.Units.SI.Parser
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                )
import           Test.Tasty.QuickCheck          ( Arbitrary )

-- A wrapper which compares angles for equality modulo 2π
newtype WrappedAngle a = WrappedAngle (MkQu_ULN [si|rad|] 'DefaultLCSU a)
  deriving (Show, Arbitrary)

instance (Floating a, Real a) => Eq (WrappedAngle a) where
  WrappedAngle x == WrappedAngle y = (x `mod'` turn) == (y `mod'` turn)

instance (Floating a, Real a) => EqProp (WrappedAngle a) where
  (=-=) = eq
