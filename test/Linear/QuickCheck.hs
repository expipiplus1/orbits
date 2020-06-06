{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.QuickCheck () where

import           Linear.V3
import           Test.QuickCheck
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                )

instance (EqProp a) => EqProp (V3 a) where
  (V3 x1 x2 x3) =-= (V3 y1 y2 y3) = (x1, x2, x3) =-= (y1, y2, y3)

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = uncurry3 V3 <$> arbitrary
  shrink (V3 x y z) = fmap (uncurry3 V3) . shrink $ (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
