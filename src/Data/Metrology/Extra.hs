{-# language QuasiQuotes #-}

module Data.Metrology.Extra where

import           Control.Applicative
import           Data.Coerce                    ( coerce )
import           Data.Constants.Mechanics.Extra ( )
import qualified Data.Fixed                    as F
                                                ( div'
                                                , divMod'
                                                , mod'
                                                )
import           Data.Metrology
import           Data.Metrology.Unsafe          ( Qu(..) )
import           Data.Units.SI.Parser
import           Linear.Epsilon
import           Linear.Metric
import           Linear.V3
import           Linear.Vector
import           Physics.Orbit.Metrology

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

rad :: Fractional a => a -> Angle a
rad = (% [si|rad|])

rdh :: Fractional a => a -> AngleH a
rdh = (% RadianHyperbolic)

qCos :: Floating a => Angle a -> Unitless a
qCos θ = quantity $ cos (θ # [si|rad|])

qSin :: Floating a => Angle a -> Unitless a
qSin θ = quantity $ sin (θ # [si|rad|])

qTan :: Floating a => Angle a -> Unitless a
qTan θ = quantity $ tan (θ # [si|rad|])

qArcTan :: Floating a => Unitless a -> Angle a
qArcTan = rad . atan . (# [si||])

qArcTan2 :: RealFloat a => Unitless a -> Unitless a -> Angle a
qArcTan2 x y = rad (atan2 (x # [si||]) (y # [si||]))

qArcCos :: Floating a => Unitless a -> Angle a
qArcCos = rad . acos . (# [si||])

qRecip
  :: forall u l a . Fractional a => Qu u l a -> Qu (Normalize ('[] @- u)) l a
qRecip = coerce (recip @a)

qTanh :: Floating a => AngleH a -> Unitless a
qTanh = quantity . tanh . (# RadianHyperbolic)

qSinh :: Floating a => AngleH a -> Unitless a
qSinh = quantity . sinh . (# RadianHyperbolic)

qCosh :: Floating a => AngleH a -> Unitless a
qCosh = quantity . cosh . (# RadianHyperbolic)

qArcCosh :: Floating a => Unitless a -> AngleH a
qArcCosh = rdh . acosh . (# [si||])

qAbs :: forall a l u . Num a => Qu u l a -> Qu u l a
qAbs = coerce (abs @a)

qCross
  :: Num n
  => V3 (Qu a l n)
  -> V3 (Qu b l n)
  -> V3 (Qu (Normalize (a @@+ Reorder b a)) l n)
qCross (V3 a b c) (V3 d e f) =
  V3 (b |*| f |-| c |*| e) (c |*| d |-| a |*| f) (a |*| e |-| b |*| d)

qNorm :: forall u l a . Floating a => V3 (Qu u l a) -> Qu u l a
qNorm = coerce (norm @V3 @a)

-- qNormalize
--   :: forall u l a . (Floating a, Epsilon a) => V3 (Qu u l a) -> V3 (Qu '[] l a)
-- qNormalize = coerce (normalize @a @V3)
qNormalize
  :: Floating n
  => V3 (Qu b l n)
  -> V3
       ( Qu
           ( Normalize
               (Normalize ('[] @- b) @@+ Reorder b (Normalize ('[] @- b)))
           )
           l
           n
       )
qNormalize x = (qRecip (qNorm x) |*|) <$> x

qDot
  :: forall u v l a. Num a
  => V3 (Qu u l a)
  -> V3 (Qu v l a)
  -> Qu (Normalize (u @@+ Reorder v u)) l a
qDot = coerce (dot @V3 @a)

qQuadrance
  :: forall u l a
   . Num a
  => V3 (Qu u l a)
  -> Qu (Normalize (u @@+ Reorder u u)) l a
qQuadrance = coerce (quadrance @V3 @a)

(|^/|) :: (Functor f, Fractional n) =>
            f (Qu b l n)
            -> Qu u l n
            -> f (Qu
                    (Normalize
                       (Normalize ('[] @- u) @@+ Reorder b (Normalize ('[] @- u))))
                    l
                    n)
x |^/| y = (qRecip y |*|) <$> x

(|^-^|)
  :: forall f u l a
   . (Additive f, Applicative f, Num a)
  => f (Qu u l a)
  -> f (Qu u l a)
  -> f (Qu u l a)
(|^-^|) = liftA2 (|-|)
