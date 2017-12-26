module Pow2
    ( rootOddPow2
    ) where

import           Util (sqr)

rootOddPow2 :: Integer -> Int -> [Integer]
rootOddPow2 a = go
  where
    go n = case n of
      _ | n <= 0 -> error "Exponent must be positive"
      1 -> [1]
      _ ->
        let xs = rootOddPow2 (a `mod` (2^(n - 1))) (n - 1)
        in concat [[x, 2^n - x] | x <- xs, sqr x `mod` 2^n == a `mod` 2^n]
