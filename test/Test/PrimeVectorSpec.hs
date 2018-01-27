{-# LANGUAGE Rank2Types #-}

module Test.PrimeVectorSpec
    ( factorizeStressTest
    , spec
    ) where

import           Prelude         hiding (toInteger)
import           Prime           (nearestPrime)
import           PrimeVector
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "PrimeVector" $ do
  describe "Num instance" $ do
    let
      pvUnProp :: (forall a. Num a => a -> a)
        -> Positive Integer -> Property
      pvUnProp f (Positive x) =
        toInteger (f $ fromInteger x :: PrimeVector) === f x
      pvBinProp :: (forall a. Num a => a -> a -> a)
        -> Positive Integer -> Positive Integer -> Property
      pvBinProp op (Positive x) (Positive y) =
        toInteger (fromInteger x `op` fromInteger y :: PrimeVector) === x `op` y
    it "should add correctly" $ property $ pvBinProp (+)
    it "should subtract correctly" $ property
      (\x y -> x > y ==> pvBinProp (-) x y)
    it "should multiply correctly" $ property $ pvBinProp (*)
    it "should abs correctly" $ property $ pvUnProp abs
    it "should signum correctly" $ property $ pvUnProp signum
  describe "Show instance" $
    it "should show correctly" $ property $ \(Positive x) ->
      show (fromInteger x :: PrimeVector) === show x
  describe "Factorize" $
    it "should easily handle ~80-bit numbers" $ factorizeStressTest 40

factorizeStressTest :: Int -> Property
factorizeStressTest factorBits = once $ property $ do
  let getPrime = nearestPrime <$> choose (2 ^ factorBits, 2 ^ (factorBits + 1))
  x <- getPrime
  y <- getPrime
  let n = fromUnsortedPrimeList [x, y]
  return $ factorize (toInteger n) === n
