{-# LANGUAGE MultiWayIf #-}

module Test.PrimeSpec
    ( spec
    ) where

import           Prime           (isPrime, mkPrime)
import qualified Prime
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "isPrime" $ do
    it "should determine if a number is prime" $
      property $ \n -> abs n > 1 ==>
        isPrime n === all (\i -> n `mod` i /= 0) [2 .. abs n - 1]
    it "should not include 0 or 1" $ do
      (-1) `shouldNotSatisfy` isPrime
      0 `shouldNotSatisfy` isPrime
      1 `shouldNotSatisfy` isPrime
    it "should handle Fermat pseudoprimes" $ do
      341 `shouldNotSatisfy` isPrime
      -561 `shouldNotSatisfy` isPrime
  describe "mkPrime" $ it "should work" $ property $ \n ->
    if
      | n <= 1 -> mkPrime n === Nothing
      | otherwise -> case mkPrime n of
          Nothing -> property $ not $ isPrime n
          Just p  -> property (isPrime n) .&&. Prime.toInteger p === n
