{-# LANGUAGE TupleSections #-}

module Lib
    ( modRoot
    ) where

import           Data.Bifunctor (second)
import           Data.Maybe     (fromJust)
import           Lift           (rootPrimePow)
import           Modulo         (crt)
import           Prime          (PrimePow)
import qualified Prime
import           PrimeVector    (PrimeVector, fromPrimes, primeDecomposition)

modRoot :: Integer -> PrimeVector -> ([Integer], PrimeVector)
modRoot a pv0 = second fst $ go subsols
  where
    subsols :: [([Integer], PrimePow)]
    subsols = [second (p,) $ rootPrimePow a (p, k) | (p, k) <- primeDecomposition pv0]
    go :: [([Integer], PrimePow)] -> ([Integer], (PrimeVector, Integer))
    go [] = ([0], (1, 1))
    go ((ns, (p, k)) : rest) =
      let (rs, (pv, pvi)) = go rest
          pk = Prime.toInteger p ^ k
      in (, (pv * fromPrimes [(p,k)], pvi * pk)) $ do
      n <- ns
      r <- rs
      return $ fst $ fromJust $ crt [(n, pk), (r, pvi)]
