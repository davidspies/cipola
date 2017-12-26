module Util
    ( PrimePow
    , sqr
    ) where

type PrimePow = (Integer, Int)

sqr :: Num a => a -> a
sqr = (^ (2 :: Int))
