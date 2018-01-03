{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators   #-}

module Test.ModuloSpec(spec) where

import           Data.List       (foldl', inits, tails)
import           Modulo          (crt)
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "crt" $
  it "should get the right result" $
    property $ \(CRTList ans) -> case crt ans of
        Nothing       -> classify False "unified" $ property (not (unifies ans))
        Just (res, m) -> classify True "unified" $
          conjoin [res `mod` n === a `mod` n | (a, n) <- ans] .&&.
          m === foldl' lcm 1 (map snd ans)

newtype CRTList = CRTList [(Integer, Integer)]
  deriving (Show)

instance Arbitrary CRTList where
  arbitrary = sized $ \s -> fmap CRTList $ listOf $ do
    n <- choose (1, 1 + toInteger s)
    a <- arbitrary
    return (a, n)
  shrink (CRTList ans) =
    map CRTList $ zipWith (++) (init $ inits ans) (tail $ tails ans)

unifies :: [(Integer, Integer)] -> Bool
unifies ans =
  and [let g = gcd m n in a `mod` g == b `mod` g | (a, m) <- ans, (b, n) <- ans]
