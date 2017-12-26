{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cipola
    ( cipola
    ) where

import           Data.List       (find, sortOn)
import           Data.Maybe      (fromJust)
import           Data.Proxy      (Proxy)
import           Data.Reflection (Reifies, reify)
import           Modulo          (E, Modulo (Modulo), modulusOf)
import qualified Modulo          as E (toInteger)
import           Prime           (Prime)
import qualified Prime
import           SquareRing
import           System.Random   (mkStdGen, randomRs)
import           Util            (sqr)

data Parity = Even | Odd

parity :: Integer -> Parity
parity n
  | odd n = Odd
  | otherwise = Even

jacobi :: Integer -> Integer -> Integer
jacobi a b
  | even b || b < 0 = error "Bad input"
  | a < 0 || a >= b = jacobi (a `mod` b) b
  | a <= 1 = a
  | otherwise = case parity a of
      Even -> case parity (a `quot` 2) of
        Even -> jacobi (a `quot` 4) b
        Odd  -> jacobi (a `quot` 2) b * jacobi2 b
      Odd -> (if  a `rem` 4 == 3 && b `rem` 4 == 3 then (-1) else 1) * jacobi b a

jacobi2 :: Integer -> Integer
jacobi2 n = case n `rem` 8 of
  1 -> 1
  3 -> -1
  5 -> -1
  7 -> 1
  _ -> error "Bad input"

-- \a p -> isPrime p && p > 2 ==> case cipola a p of
--    Nothing -> jacobi a p /= 1
--    Just x -> x ^ 2 `mod` p == a
cipola :: Integer -> Prime -> [Integer]
cipola a p = reify (Modulo $ Prime.toInteger p) $ \(_ :: Proxy s) ->
    E.toInteger <$> cipolaRing (fromInteger a :: E s
  )

cipolaRing :: forall s. Reifies s Modulo => E s -> [E s]
cipolaRing a =
  let (x, tsqr) = fromJust $
        find ((/= 1) . (`jacobi` m) . E.toInteger . snd)
          [let xe = fromInteger x' in (xe, sqr xe - a)
            | x' <- randomRs (1, m - 1) (mkStdGen 314159)]
  in if sqr x == a
    then allSolutions x
    else reify SquareRing{tsqr} $ \(_ :: Proxy t) ->
      let (Sq result z) = (Sq x 1 :: Sq s t) ^ ((m + 1) `quot` 2)
      in if z == 0 then allSolutions result else []
  where
    allSolutions x = sortOn E.toInteger [x, -x]
    m = modulusOf a
