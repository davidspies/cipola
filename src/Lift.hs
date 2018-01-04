module Lift(rootPrimePow) where

import           Cipola          (cipola)
import           Data.Foldable   (foldl')
import           Data.Reflection (reify)
import           Modulo          (Modulo (Modulo), inv, modulo)
import           Pow2            (rootOddPow2)
import           Prelude         hiding (toInteger)
import           Prime           (PrimePow)
import           ToInteger       (toInteger)
import           Util            (sqr)

liftSol :: PrimePow -> Integer -> Integer -> Integer
liftSol (prime, curPow) val sol = mult * (prime' ^ curPow) + sol
  where
    prime' = toInteger prime
    curMod = prime' ^ curPow
    k = val `div` curMod
    j = sol * sol `div` curMod
    mult = reify (Modulo prime') $ \m ->
      let n = ((k - j) `modulo` m)
          twoSolInv = inv $ (2 * sol) `modulo` m
      in toInteger $ n * twoSolInv

rootOddRelPrimePow :: Integer -> PrimePow -> [Integer]
rootOddRelPrimePow a (p, k) =
    (\sol0 -> foldl' (\prevSol i -> liftSol (p, i) a prevSol) sol0 [1..(k - 1)]) <$> cipola a p

divides :: Integer -> Integer -> Bool
divides a b = rem b a == 0
infix 4 `divides`

rootRelPrimePow :: Integer -> PrimePow -> [Integer]
rootRelPrimePow a pk@(p, k) = case toInteger p of
  2 -> rootOddPow2 a k
  _ -> rootOddRelPrimePow a pk

rootPrimePow :: Integer -> PrimePow -> ([Integer], Int)
rootPrimePow a0 pk0@(p0, k0) = go (a0 `mod` toInteger p0 ^ k0) pk0
  where
    go _ (_, 0) = ([0], 0)
    go 0 (_, 1) = ([0], 1)
    go a (p, k)
      | r == 0 = let (rs, kr) = go q (p, k - 2) in (map (* toInteger p) rs, kr + 1)
      | toInteger p `divides` a = ([], k)
      | otherwise = (rootRelPrimePow a (p, k), k)
      where
        sqrp = sqr $ toInteger p
        (q, r) = a `quotRem` sqrp
