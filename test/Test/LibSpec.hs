{-# LANGUAGE ViewPatterns #-}

module Test.LibSpec(spec) where

import           Control.Monad         (when)
import           Data.List             (sort)
import           Lib
import           Prelude               hiding (toInteger)
import           PrimeVector           (primeDecomposition)
import           Test.Hspec
import           Test.PrimeVector.Util
import           Test.QuickCheck
import           ToInteger             (toInteger)
import           Util                  (sqr)

spec :: Spec
spec = describe "modRoot" $ do
  it "should return modulo square roots" $ property $
    \(Positive n) a ->
      let (xs, toInteger -> m) = modRoot a (fromInteger n)
          rootsModN = sort [x + i * m | i <- [0 .. n `quot` m - 1], x <- xs]
      in rootsModN === [x | x <- [0 .. (n - 1)], sqr x `mod` n == a `mod` n]
  it "should return a minimal number of roots" $ property $
    \(Positive n) a ->
    let n' = fromInteger n
        (xs, _m) = modRoot a n'
    in length xs <= 2 ^ (length (primeDecomposition n') + 1)
  it "should return a modulus which is a factor of the input modulus" $ property $
    \(LargePrimeVector n) a ->
      let (_, m) = modRoot a n in toInteger m `divides` toInteger n
  it "should handle very large moduli" $ property $
    \(LargePrimeVector n) a -> sized $ \size -> do
      let (xs, m) = modRoot a n
      when (null xs) discard
      let mI = toInteger m
          nI = toInteger n
      return $ conjoin
        [ property $ \i -> sqr (i * mI + x) `mod` nI === a `mod` nI
        | x <- take size xs]

divides :: Integer -> Integer -> Bool
divides a b = rem b a == 0
infix 4 `divides`
