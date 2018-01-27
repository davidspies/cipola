{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.EllipticCurveSpec
    ( spec
    ) where

import Control.Monad (void)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy)
import Data.Reflection (Reifies, reify)
import EllipticCurve
import Modulo (E, Modulo (Modulo), modulo)
import Prelude hiding (toInteger)
import Prime (Prime)
import Test.Hspec
import Test.Modulo.Util ()
import Test.Prime.Util ()
import Test.QuickCheck
import ToInteger (toInteger)

curveTest :: Testable u
  => (Integer -> u)
  -> (forall s t. (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> u)
  -> Property
curveTest ifFactor test = property $ \(Positive n) -> curveTestModulo n ifFactor test

curveTestModPrime :: Testable u
  => (Integer -> u)
  -> (forall s t. (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> u)
  -> Property
curveTestModPrime ifFactor test = property $ \(prime :: Prime) ->
  curveTestModulo (toInteger prime) ifFactor test

curveTestModulo :: Testable u
  => Integer
  -> (Integer -> u)
  -> (forall s t. (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> u)
  -> Property
curveTestModulo n ifFactor test = property $ reify (Modulo n) $ \(_ :: Proxy s) ->
  property $ \(a :: E s) (x :: E s) (y :: E s) ->
    case pointOnCurve a x y test of
      Left f  -> ifFactor f
      Right v -> fromMaybe discard v

spec :: Spec
spec = do
  describe "pointOnCurve" $
    it "should return a point on the curve" $ curveTest discard isOnCurve
  describe "(|*|)" $ do
    it "should keep the point on the curve" $
      property $ \(NonNegative (k :: Integer)) -> curveTest discard $ \p ->
      either discard isOnCurve (k |*?| p)
    it "should not find factors of a prime" $
      property $ \(NonNegative (k :: Integer)) ->
      curveTestModPrime (const False) (isRight . (k |*?|))
    it "should be distributive for prime moduli" $
      curveTestModPrime (const $ property False) $ \p ->
      property $ \(NonNegative (i :: Integer)) (NonNegative (j :: Integer)) ->
      i |*?| p |+| j |*?| p === (i + j) |*?| p
    it "should find a factor for this case" $ (`shouldSatisfy` isLeft) $
      reify (Modulo 39) $ \m ->
      void $ pointOnCurve (15 `modulo` m) 34 11 $ \p ->
      void $ (17 :: Int) |*?| p
