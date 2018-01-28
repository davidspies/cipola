{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Lib
    ( modRoot
    ) where

import Data.Bifunctor (second)
import Data.Maybe (fromJust)
import Lift (rootPrimePow)
import Modulo (crt)
import Prelude hiding (toInteger)
import Prime (PrimePow)
import PrimeVector (PrimeVector, fromPrimes, primeDecomposition)
import ToInteger (toInteger)

modRoot :: Integer -> PrimeVector -> ([Integer], PrimeVector)
modRoot a pv0 = (
    map
      (fst . fromJust . crt)
      (cartesian [[(x, pp) | x <- xs] | (xs, toInteger -> pp) <- subsols])
  ,
    fromPrimes $ map snd subsols
  )
  where
    subsols :: [([Integer], PrimePow)]
    subsols =
      [second (p,) $ rootPrimePow a (p, k) | (p, k) <- primeDecomposition pv0]

cartesian :: [[a]] -> [[a]]
cartesian = sequence
