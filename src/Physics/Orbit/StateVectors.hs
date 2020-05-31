module Physics.Orbit.StateVectors
  ( StateVectors
  ) where

import Linear.V3

import Physics.Orbit

type Position a = V3 (Distance a)
type Velocity a = V3 (Speed a)

data StateVectors a = StateVectors
  { position :: Position a
  , velocity :: Velocity a
  }
  deriving Show

