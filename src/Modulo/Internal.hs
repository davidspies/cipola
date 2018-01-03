{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Modulo.Internal
    ( Modulo(..)
    , E
    , modulo
    , modulusOf
    , toInteger
    ) where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Reflection (Reifies (..))
import           Prelude         hiding (toInteger)

newtype Modulo = Modulo {modulus :: Integer}
newtype E s = E Integer
  deriving (Show)

instance Reifies s Modulo => Eq (E s) where
  (==) (E x) (E y) = x == y

instance Reifies s Modulo => Num (E s) where
  (+) e@(E x) (E y) = E $ (x + y) `mod` modulusOf e
  (*) e@(E x) (E y) = E $ (x * y) `mod` modulusOf e
  negate e@(E x) = E $ negate x `mod` modulusOf e
  (-) e@(E x) (E y) = E $ (x - y) `mod` modulusOf e
  abs = error "abs unsupported"
  signum = error "signum unsupported"
  fromInteger n = E $ n `mod` modulusOf (Proxy :: Proxy s)

modulo :: Reifies s Modulo => Integer -> proxy s -> E s
modulo n _ = fromInteger n

modulusOf :: Reifies s Modulo => proxy s -> Integer
modulusOf = modulus . reflect

toInteger :: E s -> Integer
toInteger (E n) = n
