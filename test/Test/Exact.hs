{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Exact
  ( Exact
  ) where

import Data.CReal       (CReal)
import Linear.Conjugate

-- | The type used for tests which require exact arithmetic. They are compared
-- at a resolution of 2^-32
type Exact = CReal 32

instance Conjugate (CReal n) where
  conjugate = id
