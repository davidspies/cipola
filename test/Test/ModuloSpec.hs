{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TypeOperators   #-}

module Test.ModuloSpec(spec) where

import           Data.List       (foldl', inits, tails)
import           Data.Reflection (reify)
import           Modulo
import           Prelude         hiding (toInteger)
import           Test.Hspec
import           Test.QuickCheck
import           ToInteger       (toInteger)

spec :: Spec
spec = do
  describe "Num instance" $ do
    let
      testBinOp :: (forall a. Num a => a -> a -> a) -> Property
      testBinOp op =
        property $ \(NonZero n) a b ->
            reify
              (Modulo n)
              (\m -> toInteger $ (a `modulo` m) `op` (b `modulo` m))
          ===
            (a `op` b) `mod` n
    it "should add correctly" $ testBinOp (+)
    it "should subtract correctly" $ testBinOp (-)
    it "should multiply correctly" $ testBinOp (*)
    it "should negate correctly" $
      property $ \(NonZero n) a ->
          reify (Modulo n) (\m -> toInteger $ negate (a `modulo` m))
        ===
          negate a `mod` n
  describe "crt" $
    it "should get the right result" $
      property $ \(CRTList ans) -> case crt ans of
          Nothing       -> classify False "unified" $
            property (not (unifies ans))
          Just (res, m) -> classify True "unified" $
            conjoin [res `mod` n === a `mod` n | (a, n) <- ans] .&&.
            m === foldl' lcm 1 (map snd ans)
  describe "inv" $
    it "should get the right result" $
      property $ \(NonZero n) a -> gcd n a == 1 ==>
        reify (Modulo n) $ \m ->
          let a' = a `modulo` m
              inva = inv a'
          in inva * a' === 1 .&&. a' * inva === 1

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
