{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PrimeVector.Util
    ( LargePrimeVector(..)
    ) where

import           Control.DeepSeq (NFData (..))
import           Data.Bifunctor  (second)
import           Data.List       (group, sort)
import           Prime           (Prime)
import           PrimeVector
import           Test.Prime.Util ()
import           Test.QuickCheck

instance Arbitrary PrimeVector where
  arbitrary = fromInteger <$> arbitrary
  shrink =
    map (fromPrimes . map (second getPositive)) .
    shrink .
    map (second Positive) . primeDecomposition

instance NFData PrimeVector where
  rnf = rnf . primeDecomposition

newtype LargePrimeVector = LargePrimeVector PrimeVector
  deriving (Show)

fromUnsortedPrimeList :: [Prime] -> PrimeVector
fromUnsortedPrimeList ps = fromPrimes $ map primeGroup $ group $ sort ps
  where
    primeGroup = \case
      [] -> error "unreachable"
      pg@(p : _) -> (p, length pg)

instance Arbitrary LargePrimeVector where
  arbitrary = LargePrimeVector . fromUnsortedPrimeList <$> arbitrary
  shrink (LargePrimeVector pv) = map LargePrimeVector (shrink pv)
