{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Modulo.Util () where

import Data.Proxy (Proxy (Proxy))
import Data.Reflection (Reifies)
import Modulo
import Test.QuickCheck

instance Reifies s Modulo => Arbitrary (E s) where
  arbitrary = fromInteger <$> choose (0, modulusOf (Proxy :: Proxy s) - 1)
  shrink = const []
