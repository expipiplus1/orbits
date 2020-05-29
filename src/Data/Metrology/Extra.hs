module Data.Metrology.Extra
  ( mod'
  , div'
  , divMod'
  ) where

import           Data.Coerce                    ( coerce )
import qualified Data.Fixed                    as F
                                                ( div'
                                                , divMod'
                                                , mod'
                                                )
import           Data.Metrology
import           Data.Metrology.Unsafe          ( Qu(..) )

mod' :: forall a u l . Real a => Qu u l a -> Qu u l a -> Qu u l a
mod' = coerce (F.mod' :: a -> a -> a)

div'
  :: forall a b u v l
   . (Real a, Integral b)
  => Qu u l a
  -> Qu v l a
  -> Qu (Normalize (u @- v)) l b
div' = coerce (F.div' :: a -> a -> b)

divMod'
  :: forall a b u l
   . (Real a, Integral b)
  => Qu u l a
  -> Qu u l a
  -> (Qu '[] l b, Qu u l a)
divMod' = coerce (F.divMod' :: a -> a -> (b, a))
