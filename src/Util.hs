module Util
  ( Parity (..),
    parity,
    sqr,
    withPhantom,
  )
where

data Parity = Even | Odd

parity :: Integer -> Parity
parity n
  | odd n = Odd
  | otherwise = Even

sqr :: Num a => a -> a
sqr = (^ (2 :: Int))

withPhantom :: a s -> b s -> a s
withPhantom = const
