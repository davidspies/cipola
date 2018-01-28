{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module EllipticCurve
  ( EC(Zero)
  , EEC
  , EPOC
  , EllipticCurve
  , OrFactor
  , PointOnCurve(..)
  , (|+|)
  , (|+?|)
  , (|*|)
  , (|*?|)
  , isOnCurve
  , negate
  , point
  , pointOnCurve
  , randomPoints
  ) where

import Control.Monad (join)
import Data.Maybe (isJust)
import Data.Reflection (Reifies, reflect, reify)
import Modulo
import Prelude hiding (negate, toInteger)
import qualified Prelude
import System.Random (Random (..), mkStdGen)
import ToInteger (toInteger)
import Util (sqr)

data EllipticCurve s = EllipticCurve {a :: !(E s), b :: !(E s)}
  deriving (Show)
deriving instance Reifies s Modulo => Eq (EllipticCurve s)

data EC s t = Zero | EC{x :: !(E s), y :: !(E s)}
  deriving (Show)
deriving instance (Reifies s Modulo) => Eq (EC s t)

type OrFactor s = Either Integer
type EEC s t = OrFactor s (EC s t)

ec :: proxy t -> E s -> E s -> EC s t
ec _ x y = EC{x, y}

(|+?|) :: (Reifies s Modulo, Reifies t (EllipticCurve s))
  => EC s t -> EC s t -> EEC s t
(|+?|) = go
  where
    go p Zero = Right p
    go Zero q = Right q
    go p@(EC xp yp) (EC xq yq)
      | xp == xq = if
        | yp == -yq -> Right Zero
        | yp == yq -> computeResult
        | otherwise -> Left $ gcd n (toInteger (yp + yq))
      | otherwise = computeResult
      where
        Modulo n = reflect xp
        (snum, sdenom)
          | xp == xq = (3 * sqr xp + a, 2 * yp)
          | otherwise = (yq - yp, xq - xp)
        EllipticCurve{a} = reflect p
        computeResult = do
          i <- tryInv sdenom
          let
            s = snum * i
            xr = sqr s - xp - xq
            yr = yp + s * (xr - xp)
          return $ EC xr (-yr)
infixl 6 |+?|

(|+|) :: (Reifies s Modulo, Reifies t (EllipticCurve s))
  => EEC s t -> EEC s t -> EEC s t
(|+|) p q = join $ (|+?|) <$> p <*> q
infixl 6 |+|

newtype AsNum s t = AsNum {unNum :: EEC s t}

instance (Reifies s Modulo, Reifies t (EllipticCurve s)) => Num (AsNum s t) where
  (+) = error "unused"
  (*) (AsNum x) (AsNum y) = AsNum $ x |+| y
  negate = error "unused"
  abs = error "unused"
  signum = error "unused"
  fromInteger = \case
    1 -> AsNum (Right Zero)
    _ -> error "unused"

(|*?|) :: (Integral a, Reifies s Modulo, Reifies t (EllipticCurve s))
  => a -> EC s t -> EEC s t
(|*?|) n x = unNum $ AsNum (Right x) ^ n
infixl 7 |*?|

(|*|) :: (Integral a, Reifies s Modulo, Reifies t (EllipticCurve s))
  => a -> EEC s t -> EEC s t
(|*|) n x = x >>= (|*?|) n
infixl 7 |*|

negate :: (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> EC s t
negate = \case
  Zero -> Zero
  EC{x, y} -> EC{x, y = -y}

pointOnCurve :: Reifies s Modulo => E s -> E s -> E s
             -> (forall t. Reifies t (EllipticCurve s) => EC s t -> a)
             -> OrFactor s (Maybe a)
pointOnCurve a x y continuation =
  fmap (`reify` (\proxy -> continuation (ec proxy x y))) <$>
  mkCurve a (sqr y - (sqr x + a) * x)

-- Should always be true for any point that can be generated safely.
isOnCurve :: (Reifies s Modulo, Reifies t (EllipticCurve s)) => EC s t -> Bool
isOnCurve p = case p of
    EC{x, y} -> isJust $ point p x y
    Zero     -> True

mkCurve :: Reifies s Modulo
  => E s -> E s -> OrFactor s (Maybe (EllipticCurve s))
mkCurve a b
  | discriminant == 0 = Right Nothing
  | g /= 1 = Left g
  | otherwise = Right $ Just EllipticCurve{a, b}
  where
    n = modulusOf a
    discriminant = toInteger $ 4 * a ^ (3 :: Int) + 27 * b ^ (2 :: Int)
    g = gcd discriminant n

point :: (Reifies s Modulo, Reifies t (EllipticCurve s))
  => proxy t -> E s -> E s -> Maybe (EC s t)
point proxy x y
    | (sqr x + a) * x + b == sqr y = Just $ ec proxy x y
    | otherwise                    = Nothing
  where
    EllipticCurve{a, b} = reflect proxy

data PointOnCurve s = forall t. Reifies t (EllipticCurve s)
  => PointOnCurve (EC s t)

type EPOC s = OrFactor s (PointOnCurve s)

randomPoints :: Reifies s Modulo => [EPOC s]
randomPoints = randoms (mkStdGen 2718281828)

instance Reifies s Modulo => Random (EPOC s) where
  randomR = error "Unordered"
  random gen = case pointOnCurve a x y PointOnCurve of
      Left f         -> (Left f, g3)
      Right Nothing  -> random g3
      Right (Just r) -> (Right r, g3)
    where
      (a, g1) = random gen
      (x, g2) = random g1
      (y, g3) = random g2
