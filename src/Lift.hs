{-# LANGUAGE ScopedTypeVariables #-}

module Lift where

import           Data.Proxy      (Proxy)
import           Data.Reflection (reify)
import           Modulo          (E, Modulo (Modulo), inv, toInteger)
import           Prelude         hiding (toInteger)

liftSol :: Integer -> Int -> Integer -> Integer -> Integer
liftSol prime curPow val sol = mult * (prime ^ curPow) + sol
  where
    curMod = prime ^ curPow
    k = val `div` curMod
    j = sol * sol `div` curMod
    mult = reify (Modulo prime) $ \(_ :: Proxy s) ->
      let n = (fromInteger (k - j) :: E s)
          twoSolInv = inv $ fromInteger (2 * sol)
      in toInteger $ n * twoSolInv
