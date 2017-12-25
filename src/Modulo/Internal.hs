{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Modulo.Internal
    ( Modulo(..)
    , E
    , modulusOf
    , toInteger
    ) where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Reflection (Reifies (..))
import           Prelude         hiding (toInteger)

newtype Modulo = Modulo {modulus :: Integer}
newtype E s = E Integer

instance Reifies s Modulo => Eq (E s) where
  (==) (E x) (E y) = x == y

instance Reifies s Modulo => Num (E s) where
  (+) e@(E x) (E y) = E $ (x + y) `rem` modulusOf e
  (*) e@(E x) (E y) = E $ (x * y) `rem` modulusOf e
  negate e@(E x) = case x of
    0 -> 0
    _ -> E $ modulusOf e - x
  (-) e@(E x) (E y) = E $ x - y + (if x < y then modulusOf e else 0)
  abs = error "abs unsupported"
  signum = error "signum unsupported"
  fromInteger n = E $ n `mod` modulusOf (Proxy :: Proxy s)

modulusOf :: Reifies s Modulo => proxy s -> Integer
modulusOf = modulus . reflect

toInteger :: E s -> Integer
toInteger (E n) = n
