{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ring where

import           Control.Monad.Reader (Reader)
import           Prelude              hiding (Num (..), (^))

class Ring r where
  data Elem r
  (+) :: Elem r -> Elem r -> Reader r (Elem r)
  (*) :: Elem r -> Elem r -> Reader r (Elem r)
  one :: Reader r (Elem r)
  negate :: Elem r -> Reader r (Elem r)
  (-) :: Elem r -> Elem r -> Reader r (Elem r)
  (-) x y = (x +) =<< negate y
  zero :: Reader r (Elem r)
  zero = do
    v <- one
    v - v
  (^) :: Elem r -> Integer -> Reader r (Elem r)
  (^) x y = case compare y 0 of
    LT -> error "Negative exponent"
    EQ -> one
    GT -> do
      let (q, r) = y `quotRem` 2
      rest <- x * x >>= (^ q)
      if r == 1 then x * rest else return rest
  e :: Integer -> Reader r (Elem r)
  e n = case compare n 0 of
    LT -> negate =<< e' (-n)
    EQ -> zero
    GT -> e' n
    where
      e' :: Integer -> Reader r (Elem r)
      e' 1 = one
      e' n' = do
        let (q, r) = n' `quotRem` 2
        x <- e' q
        y <- x + x
        if r == 1 then one >>= (y +) else return y
