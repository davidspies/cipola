{-# LANGUAGE ScopedTypeVariables #-}

module Lift where

import           Cipola          (cipola)
import           Data.Foldable   (foldl')
import           Data.List       (sort)
import           Data.Proxy      (Proxy)
import           Data.Reflection (reify)
import           Modulo          (E, Modulo (Modulo), inv, toInteger)
import           Prelude         hiding (toInteger)
import           Util            (PrimePow)

liftSol :: PrimePow -> Integer -> Integer -> Integer
liftSol (prime, curPow) val sol = mult * (prime ^ curPow) + sol
  where
    curMod = prime ^ curPow
    k = val `div` curMod
    j = sol * sol `div` curMod
    mult = reify (Modulo prime) $ \(_ :: Proxy s) ->
      let n = (fromInteger (k - j) :: E s)
          twoSolInv = inv $ fromInteger (2 * sol)
      in toInteger $ n * twoSolInv

rootRelPrimePow :: Integer -> PrimePow -> [Integer]
rootRelPrimePow a (p, k) = sort $
    (\sol0 -> foldl' (\prevSol i -> liftSol (p, i) a prevSol) sol0 [1..(k - 1)]) <$> cipola a p
