{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrimeVector
    ( PrimeVector
    , isPrime
    , primeDecomposition
    , toInteger
    ) where

import           Data.List       (find)
import           Data.Proxy      (Proxy)
import           Data.Reflection (reify)
import           Modulo          (E, Modulo (Modulo))
import           Prelude         hiding (toInteger)

newtype PrimeVector = PrimeVector [(Integer, Int)]

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
      go f n | sqr f > n = [(n, 1)]
      go f n = let (k, leftover) = f `multiplicityIn` n in
        (if k > 0 then ((f, k) :) else id) $ go (f + 1) leftover

sqr :: Num a => a -> a
sqr = (^ (2 :: Int))

multiplicityIn :: Integer -> Integer -> (Int, Integer)
multiplicityIn f = go 0
  where
    go !accum n
      | r == 0 = go (accum + 1) q
      | otherwise = (accum, n)
      where
        (q, r) = n `quotRem` f

primeDecomposition :: PrimeVector -> [(Integer, Int)]
primeDecomposition (PrimeVector fs) = fs

toInteger :: PrimeVector -> Integer
toInteger (PrimeVector fs) = product [f ^ n | (f, n) <- fs]

-- Uses Miller-Rabin primality-testing. Either determines that a number is either prime or a
-- counterexample to ERH (Extended Riemann Hypothesis).
isPrime :: Integer -> Bool
isPrime n0
  | n0 < (-1) = go (negate n0)
  | n0 > 1 = go n0
  | otherwise = False
  where
    go n = all checkCandidate candidates
      where
        candidates = [2..(min (n - 1) (sqr $ ceillog2 $ fromInteger n))]
    checkCandidate a = reify (Modulo n0) $ \(_ :: Proxy s) ->
      let t0 = ((fromInteger a :: E s) ^ m)
      in t0 == 1 || t0 == -1 ||
        case find (\x -> x == 1 || x == -1) (take k $ iterate sqr t0) of
            Nothing   -> False
            Just (-1) -> True
            Just _    -> False
    (m, k) = checkOrders (n0 - 1) 0
      where
        checkOrders n !count
          | odd n = (n, count)
          | otherwise = checkOrders (n `quot` 2) (count + 1)

ceillog2 :: Integer -> Integer
ceillog2 n0
  | n0 <= 0 = error "Must be positive"
  | otherwise = go 0 n0
  where
    go !accum = \case
      1 -> accum
      n -> go (accum + 1) ((n + 1) `quot` 2)
