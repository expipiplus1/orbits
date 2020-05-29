module Data.Constants.Mechanics.Extra
  ( turn
  , halfTurn
  , addRad
  , delRad
  ) where

import           Data.Coerce
import qualified Data.Dimensions.SI            as D
import           Data.Metrology
import           Data.Metrology.SI              ( )
import           Data.Metrology.Unsafe
import           Data.Units.SI

type PlaneAngle = MkQu_DLN D.PlaneAngle DefaultLCSU

-- | One complete revolution in radians
turn :: Floating a => PlaneAngle a
turn = 2 |*| halfTurn

-- | π radians
halfTurn :: Floating a => PlaneAngle a
halfTurn = pi % Radian

-- | Multiply by 1 radian
addRad
  :: Qu b 'DefaultLCSU a
  -> Qu (Normalize ('[ 'F D.PlaneAngle One] @+ b)) 'DefaultLCSU a
addRad = coerce

-- | Divide by 1 radian
delRad
  :: Qu u 'DefaultLCSU a
  -> Qu (Normalize (u @- '[ 'F D.PlaneAngle One])) 'DefaultLCSU a
delRad = coerce
