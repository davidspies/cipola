module Lib
    ( cipola
    , rootPow2
    ) where

import           Cipola

-- Finds (xs, n') st. forall v: v `mod` 2^n' `elem` xs <-> v^2 `mod` 2^n == a
-- and length xs <= 4
rootPow2 :: Integer -> Int -> ([Integer], Int)
rootPow2 a n = case n of
  _ | n < 0 -> error "Exponent must be positive"
  0 -> ([0], 0)
  1 -> ([a `mod` 2], fromInteger $ a `mod` 2)
  _ -> case parity a of
    Even ->
      case parity (a `quot` 2) of
        Even ->
          let (xs, n') = rootPow2 (a `quot` 4) (n - 2)
          in (map (2 *) xs, n' + 1)
        Odd -> ([], n)
    Odd ->
      let (xs, _) = rootPow2 (a `mod` (2^(n - 1))) (n - 1)
      in (concat [[x, 2^n - x] | x <- xs, x^(2 :: Int) `mod` 2^n == a `mod` 2^n], n)
