{-# LANGUAGE ViewPatterns #-}

module Test.LibSpec where

import           Data.List       (sort)
import           Lib
import           PrimeVector     (primeDecomposition)
import qualified PrimeVector
import           Test.Hspec
import           Test.QuickCheck (property, (===), (==>))

spec :: Spec
spec = describe "modRoot" $ do
  it "Should return modulo square roots" $ property $
    \n -> n > 0 ==> \a ->
      let (xs, PrimeVector.toInteger -> m) = modRoot a (fromInteger n)
          rootsModN = sort [x + i * m | i <- [0 .. n `quot` m - 1], x <- xs]
      in rootsModN === [x | x <- [0 .. (n - 1)], x ^ (2 :: Int) `mod` n == a `mod` n]
  it "Should return a minimal number of roots" $ property $
    \n -> n > 0 ==> \a ->
    let n' = fromInteger n
        (xs, _m) = modRoot a n'
    in length xs <= 2 ^ (length (primeDecomposition n') + 1)
