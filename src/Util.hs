module Util
    ( sqr
    ) where

sqr :: Num a => a -> a
sqr = (^ (2 :: Int))
