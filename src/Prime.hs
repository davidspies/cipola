{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prime
    ( Prime
    , PrimePow
    , isPrime
    , mkPrime
    , toInteger
    ) where

import           Data.List       (find)
import           Data.Proxy      (Proxy)
import           Data.Reflection (reify)
import           Modulo          (E, Modulo (Modulo))
import           Prelude         hiding (toInteger)
import           Prime.Internal
import           Util            (sqr)

type PrimePow = (Prime, Int)

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
        checkCandidate a = reify (Modulo n) $ \(_ :: Proxy s) ->
          let t0 = ((fromInteger a :: E s) ^ m)
          in t0 == 1 || t0 == -1 ||
            case find (\x -> x == 1 || x == -1) (take k $ iterate sqr t0) of
                Nothing   -> False
                Just (-1) -> True
                Just _    -> False
        (m, k) = checkOrders (n - 1) 0
          where
            checkOrders n' !count
              | odd n' = (n', count)
              | otherwise = checkOrders (n' `quot` 2) (count + 1)

mkPrime :: Integer -> Maybe Prime
mkPrime n
  | n > 0 && isPrime n = Just (Prime n)
  | otherwise = Nothing

ceillog2 :: Integer -> Integer
ceillog2 n0
  | n0 <= 0 = error "Must be positive"
  | otherwise = go 0 n0
  where
    go !accum = \case
      1 -> accum
      n -> go (accum + 1) ((n + 1) `quot` 2)

toInteger :: Prime -> Integer
toInteger (Prime p) = p
