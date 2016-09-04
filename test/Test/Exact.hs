{-# LANGUAGE DataKinds #-}

module Test.Exact
  ( Exact
  ) where

import Data.CReal (CReal)

-- | The type used for tests which require exact arithmetic. They are compared
-- at a resolution of 2^-32
type Exact = CReal 32

