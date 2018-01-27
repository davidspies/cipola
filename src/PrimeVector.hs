{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrimeVector
    ( PrimeVector
    , fromPrimes
    , fromUnsortedPrimeList
    , primeDecomposition
    , toInteger
    ) where

import Data.List (group, sort)
import Factorize (factorize)
import Prelude hiding (toInteger)
import Prime (Prime, PrimePow)
import ToInteger (ToInteger (..))

newtype PrimeVector = PrimeVector [PrimePow]
  deriving (Eq)

instance Num PrimeVector where
  (+) x y = fromInteger (toInteger x + toInteger y)
  (*) (PrimeVector xs0) (PrimeVector ys0) = PrimeVector (go xs0 ys0)
    where
      go [] ys = ys
      go xs [] = xs
      go ((x, m) : xs) ((y, n) : ys) = case compare x y of
        LT -> (x, m) : go xs ((y, n) : ys)
        EQ -> (x, m + n) : go xs ys
        GT -> (y, n) : go ((x, m) : xs) ys
  negate _ = error "Cannot negate"
  (-) x y = fromInteger (toInteger x - toInteger y)
  abs = id
  signum = const 1
  fromInteger = fromUnsortedPrimeList . factorize

instance Show PrimeVector where
  show pv = show $ toInteger pv

primeDecomposition :: PrimeVector -> [PrimePow]
primeDecomposition (PrimeVector fs) = fs

instance ToInteger PrimeVector where
  toInteger (PrimeVector fs) = product [toInteger f ^ n | (f, n) <- fs]

fromPrimes :: [PrimePow] -> PrimeVector
fromPrimes xs0 = validate xs0 `seq` PrimeVector xs0
  where
    validate [] = ()
    validate ((_, n0) : _) | n0 <= 0 = error "Exponents not all positive"
    validate ((p0, _) : (p1, _) : _) | toInteger p1 < toInteger p0 =
      error "Terms out of order"
    validate ((_, _) : xs) = validate xs

fromUnsortedPrimeList :: [Prime] -> PrimeVector
fromUnsortedPrimeList ps = fromPrimes $ map primeGroup $ group $ sort ps
  where
    primeGroup = \case
      [] -> error "unreachable"
      pg@(p : _) -> (p, length pg)
