{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
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
    -- ** Trigonometry
  , sin'
  , cos'
  , acos'
  , atan2'
    -- ** Linear
  , cross'
  , dot'
  , norm'
  , quadrance'
  , signorm'
  , (^/:)
  , (^-^:)
  , (^*^:)
    -- ** Utils
  , unsafeMapUnit
  ) where

import           Data.Coerce                  (coerce)
import qualified Data.Fixed                   as F (div', divMod', mod')
import           Data.UnitsOfMeasure
import           Data.UnitsOfMeasure.Internal (Quantity (..))
import           Linear.Metric
import           Linear.V3
import           Linear.Vector

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

sin' :: forall a.
        Floating a
     => Quantity a [u|rad|] -> Quantity a One
sin' = coerce (sin :: a -> a)

cos' :: forall a.
        Floating a
     => Quantity a [u|rad|] -> Quantity a One
cos' = coerce (cos :: a -> a)

acos' :: forall a.
         Floating a
      => Quantity a One -> Quantity a [u|rad|]
acos' = coerce (acos :: a -> a)

atan2' :: forall a.
          RealFloat a
       => Quantity a One -> Quantity a One -> Quantity a [u|rad|]
atan2' = coerce (atan2 :: a -> a -> a)

cross' :: forall a u v.
          Num a
       => V3 (Quantity a u) -> V3 (Quantity a v) -> V3 (Quantity a (u *: v))
cross' = coerce (cross :: V3 a -> V3 a -> V3 a)

dot' :: (Metric f, Num a)
     => f (Quantity a u) -> f (Quantity a v) -> Quantity a (u *: v)
dot' a b = MkQuantity ((unQuantity <$> a) `dot` (unQuantity <$> b))

norm' :: forall f a u.
         (Metric f, Floating a)
      => f (Quantity a u) -> Quantity a u
norm' = MkQuantity . norm . fmap unQuantity

quadrance' :: forall f a u.
              (Metric f, Floating a)
           => f (Quantity a u) -> Quantity a (u *: u)
quadrance' = MkQuantity . quadrance . fmap unQuantity

signorm' :: forall f a u.
            (Metric f, Floating a)
         => f (Quantity a u) -> f (Quantity a u)
signorm' = fmap MkQuantity . signorm . fmap unQuantity

-- | Compute division by a scalar on the right, annotated with units
(^/:) :: (Functor f, Fractional a)
      => f (Quantity a u) -> Quantity a v -> f (Quantity a (u /: v))
v ^/: s = MkQuantity <$> ((unQuantity <$> v) ^/ unQuantity s)
infixl 7 ^/:

-- | Compute the difference between two vectors on quantities
(^-^:) :: (Additive f, Num a)
       => f (Quantity a u) -> f (Quantity a u) -> f (Quantity a u)
a ^-^: b = MkQuantity <$> ((unQuantity <$> a) ^-^ (unQuantity <$> b))
infixl 6 ^-^:

-- | Compute the difference between two vectors on quantities
(^*^:) :: (Additive f, Num a)
       => f (Quantity a u) -> f (Quantity a v) -> f (Quantity a (u *: v))
a ^*^: b = MkQuantity <$> liftU2 (*) (unQuantity <$> a) (unQuantity <$> b)
infixl 7 ^*^:

unsafeMapUnit :: (a -> b) -> Quantity a u -> Quantity b u
unsafeMapUnit f (MkQuantity x) = MkQuantity (f x)
