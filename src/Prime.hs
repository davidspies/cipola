{-# LANGUAGE BangPatterns #-}

module Prime
    ( Prime
    , PrimePow
    , isPrime
    , mkPrime
    , toInteger
    ) where

import           Data.List       (find)
import           Data.Reflection (reify)
import           Modulo          (Modulo (Modulo), modulo)
import           Prelude         hiding (toInteger)
import qualified Prelude         as P
import           Prime.Internal
import           Util            (Parity (..), parity, sqr)

type PrimePow = (Prime, Int)

isPrime :: Integer -> Bool
isPrime 0  = False
isPrime n0 = positiveIsPrime (abs n0)

-- Uses Miller-Rabin primality-testing. Determines that a number is either prime or a counterexample
-- to ERH (Extended Riemann Hypothesis).
positiveIsPrime :: Integer -> Bool
positiveIsPrime 1 = False
positiveIsPrime n = reify (Modulo n) $ \m ->
  let
    candidates = map (`modulo` m)
      [2..(min (n - 1) (sqr $ P.toInteger $ ceillog2 n))]
    checkCandidate a =
      let t0 = a ^ r
      in t0 == 1 || t0 == -1 ||
        case find (\x -> x == 1 || x == -1) (take k $ iterate sqr t0) of
            Nothing   -> False
            Just (-1) -> True
            Just _    -> False
  in all checkCandidate candidates
  where
    (r, k) = checkOrders (n - 1) 0
      where
        checkOrders n' !count = case parity n' of
          Odd  -> (n', count)
          Even -> checkOrders (n' `quot` 2) (count + 1)

mkPrime :: Integer -> Maybe Prime
mkPrime n
  | n > 0 && positiveIsPrime n = Just (Prime n)
  | otherwise = Nothing

ceillog2 :: Integer -> Int
ceillog2 n0
  | n0 <= 0 = error "Must be positive"
  | otherwise = length (takeWhile (> 1) $ iterate (\n -> (n + 1) `quot` 2) n0)

toInteger :: Prime -> Integer
toInteger (Prime p) = p
