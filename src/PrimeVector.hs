{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrimeVector
    ( PrimeVector
    , fromPrimes
    , primeDecomposition
    , toInteger
    ) where

import           Prelude        hiding (toInteger)
import           Prime          (PrimePow)
import qualified Prime
import           Prime.Internal (Prime (Prime))
import           Util           (sqr)

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
  -- TODO Pollard Rho or Elliptic Curve factorization
  fromInteger n0
    | n0 <= 0 = error "Positive only"
    | otherwise = PrimeVector $ go 2 n0
    where
      go _ 1 = []
      go f n | sqr f > n = [(Prime n, 1)]
      go f n = let (k, leftover) = f `multiplicityIn` n in
        (if k > 0 then ((Prime f, k) :) else id) $ go (f + 1) leftover

instance Show PrimeVector where
  show pv = show $ toInteger pv

multiplicityIn :: Integer -> Integer -> (Int, Integer)
multiplicityIn f = go 0
  where
    go !accum n
      | r == 0 = go (accum + 1) q
      | otherwise = (accum, n)
      where
        (q, r) = n `quotRem` f

primeDecomposition :: PrimeVector -> [PrimePow]
primeDecomposition (PrimeVector fs) = fs

toInteger :: PrimeVector -> Integer
toInteger (PrimeVector fs) = product [Prime.toInteger f ^ n | (f, n) <- fs]

fromPrimes :: [PrimePow] -> PrimeVector
fromPrimes xs0 = validate xs0 `seq` PrimeVector xs0
  where
    validate [] = ()
    validate ((_, n0) : _) | n0 <= 0 = error "Exponents not all positive"
    validate ((p0, _) : (p1, _) : _) | Prime.toInteger p1 < Prime.toInteger p0 =
      error "Terms out of order"
    validate ((_, _) : xs) = validate xs
