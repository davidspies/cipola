module Util
    ( Parity(..)
    , parity
    , sqr
    ) where

data Parity = Even | Odd

parity :: Integer -> Parity
parity n
  | odd n = Odd
  | otherwise = Even

sqr :: Num a => a -> a
sqr = (^ (2 :: Int))
