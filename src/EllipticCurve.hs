{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module EllipticCurve
  ( EC
  , ECView(..)
  , EllipticCurve(..)
  , (|+|)
  , (|*|)
  , negate
  , point
  , pointOnCurve
  , view
  , zero
  ) where

import           Data.Reflection (Reifies, reflect, reify)
import           Modulo
import           Prelude         hiding (negate, toInteger)
import qualified Prelude
import           ToInteger       (toInteger)
import           Util            (sqr)

data EllipticCurve s = EllipticCurve {a :: E s, b :: E s}
  deriving (Show)
deriving instance Reifies s Modulo => Eq (EllipticCurve s)

data ECView s t = Zero | EC{x :: E s, y :: E s} | ModulusFactor Integer
  deriving (Show)
deriving instance Reifies s Modulo => Eq (ECView s t)
newtype EC s t = EC' (ECView s t)
  deriving (Show)
deriving instance Reifies s Modulo => Eq (EC s t)

ec :: proxy t -> E s -> E s -> EC s t
ec _ x y = EC' EC{x, y}

(|+|) :: (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> EC s t -> EC s t
(|+|) (EC' p0) (EC' q0) = EC' $ go p0 q0
  where
    go p Zero = p
    go Zero q = q
    go (ModulusFactor f) _ = ModulusFactor f
    go _ (ModulusFactor f) = ModulusFactor f
    go p@(EC xp yp) q@(EC xq yq)
      | xp == xq && yp == -yq = Zero
      | xp == xq && yp /= yq = error "Something's gone wrong"
      | gcd n (toInteger sdenom) /= 1 = ModulusFactor $ gcd n (toInteger (xq - xp)) -- TODO GCD and inverse together
      | otherwise = EC xr (-yr)
      where
        Modulo n = reflect xp
        (snum, sdenom)
          | p == q = (3 * sqr xp + a, 2 * yp)
          | otherwise = (yq - yp, xq - xp)
        s = snum * inv sdenom
        xr = sqr s - xp - xq
        yr = yp + s * (xr - xp)
        EllipticCurve{a} = reflect p
infixl 6 |+|

newtype AsNum s t = AsNum {unNum :: EC s t}

instance (Reifies s Modulo, Reifies t (EllipticCurve s)) => Num (AsNum s t) where
  (+) = error "unused"
  (*) (AsNum x) (AsNum y) = AsNum $ x |+| y
  negate = error "unused"
  abs = error "unused"
  signum = error "unused"
  fromInteger = \case
    1 -> AsNum zero
    _ -> error "unused"

(|*|) :: (Integral a, Reifies s Modulo, Reifies t (EllipticCurve s))
  => a -> EC s t -> EC s t
(|*|) n x = unNum $ AsNum x ^ n
infixl 7 |*|

negate :: (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> EC s t
negate (EC' p) = EC' $ go p
  where
    go Zero              = Zero
    go EC{x, y}          = EC{x, y = -y}
    go (ModulusFactor f) = ModulusFactor f

pointOnCurve :: Reifies s Modulo => E s -> E s -> E s
             -> (forall t. Reifies t (EllipticCurve s) => EC s t -> a) -> a
pointOnCurve a x y continuation =
  reify curve $ \proxy -> continuation (ec proxy x y)
  where
    curve = EllipticCurve{a, b = sqr y - (sqr x + a) * x}

view :: EC s t -> ECView s t
view (EC' v) = v

zero :: EC s t
zero = EC' Zero

point :: (Reifies s Modulo, Reifies t (EllipticCurve s))
  => proxy t -> E s -> E s -> Maybe (EC s t)
point proxy x y
    | (sqr x + a) * x + b == sqr y = Just $ ec proxy x y
    | otherwise                    = Nothing
  where
    EllipticCurve{a, b} = reflect proxy
