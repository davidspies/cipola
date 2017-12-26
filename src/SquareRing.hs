{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module SquareRing where

import           Data.Reflection (Reifies, reflect)
import           Modulo

newtype SquareRing s = SquareRing {tsqr :: E s}
data Sq s t = Sq !(E s) !(E s)
  deriving (Show)

instance (Reifies s Modulo, Reifies t (SquareRing s)) => Eq (Sq s t) where
  (==) (Sq rx ix) (Sq ry iy) = rx == ry && ix == iy

instance (Reifies s Modulo, Reifies t (SquareRing s)) => Num (Sq s t) where
  (+) (Sq rx ix) (Sq ry iy) = Sq (rx + ry) (ix + iy)
  (*) e@(Sq rx ix) (Sq ry iy) = Sq (rx * ry + tsqrOf e * ix * iy) (rx * iy + ry * ix)
  negate (Sq r i) = Sq (negate r) (negate i)
  (-) (Sq rx ix) (Sq ry iy) = Sq (rx - ry) (ix - iy)
  abs = error "abs unsupported"
  signum = error "signum unsupported"
  fromInteger n = Sq (fromInteger n) 0

tsqrOf :: (Reifies s Modulo, Reifies t (SquareRing s)) => proxy t -> E s
tsqrOf = tsqr . reflect
