{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.EllipticCurveSpec
    ( spec
    ) where

import           Data.Maybe       (isJust)
import           Data.Proxy       (Proxy)
import           Data.Reflection  (Reifies, reify)
import           EllipticCurve
import           Modulo           (E, Modulo (Modulo))
import           Prelude          hiding (toInteger)
import           Prime            (Prime)
import           Test.Hspec
import           Test.Modulo.Util ()
import           Test.Prime.Util  ()
import           Test.QuickCheck
import           ToInteger        (toInteger)

-- Should always be true for any point that can be generated safely.
isOnCurve :: (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> Bool
isOnCurve p0 = case view p0 of
    EC{x, y}        -> isJust $ point p0 x y
    Zero            -> True
    ModulusFactor _ -> True

foundFactor :: EC s t -> Bool
foundFactor p = case view p of
  (ModulusFactor _) -> True
  _                 -> False

curveTest :: Testable u
  => (forall s t. (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> u)
  -> Property
curveTest test = property $ \(Positive n) -> curveTestModulo n test

curveTestModPrime :: Testable u
  => (forall s t. (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> u)
  -> Property
curveTestModPrime test = property $ \(prime :: Prime) ->
  curveTestModulo (toInteger prime) test

curveTestModulo :: Testable u
  => Integer
  -> (forall s t. (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> u)
  -> Property
curveTestModulo n test = property $ reify (Modulo n) $ \(_ :: Proxy s) ->
  property $ \(a :: E s) (x :: E s) (y :: E s) -> pointOnCurve a x y test

spec :: Spec
spec = do
  describe "pointOnCurve" $
    it "should return a point on the curve" $ curveTest isOnCurve
  describe "(|*|)" $ do
    it "should keep the point on the curve" $
      property $ \(NonNegative (k :: Integer)) -> curveTest $ \p ->
      isOnCurve (k |*| p)
    it "should not find factors of a prime" $
      property $ \(NonNegative (k :: Integer)) ->
      curveTestModPrime $ \p -> not (foundFactor $ k |*| p)
    it "should be distributive for prime moduli" $ curveTestModPrime $ \p ->
      property $ \(NonNegative (i :: Integer)) (NonNegative (j :: Integer)) ->
      i |*| p |+| j |*| p === (i + j) |*| p
