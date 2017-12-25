{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}

module ModRing where

import           Control.Monad.Reader  (Reader, ReaderT (ReaderT), ask, asks)
import           Data.Functor.Identity (Identity (Identity))
import           Ring                  (Elem, Ring)
import qualified Ring

newtype ModRing = ModRing {modulus :: Integer}

instance Ring ModRing where
  newtype Elem ModRing = EM {unEM :: Integer}
    deriving (Eq)
  zero = return $ EM 0
  one = (EM . (1 `mod`)) <$> asks modulus
  negate (EM k) = do
    n <- asks modulus
    return $ EM (negate k `mod` n)
  (+) (EM kl) (EM kr) = do
    n <- asks modulus
    return $ EM ((kl + kr) `mod` n)
  (*) (EM kl) (EM kr) = do
    ModRing{modulus} <- ask
    return $ EM ((kl * kr) `mod` modulus)
  (-) (EM kl) (EM kr) = do
    n <- asks modulus
    return $ EM ((kl - kr) `mod` n)
  e i = do
    n <- asks modulus
    return $ EM $ i `mod` n

instance Show (Elem ModRing) where
  show (EM x) = show x

toInteger :: Elem ModRing -> Integer
toInteger (EM n) = n

inv :: Elem ModRing -> Reader ModRing (Elem ModRing)
inv (EM a) = ReaderT (\(ModRing n) -> Identity $ EM $ inv' a n)

inv' :: Integer -> Integer -> Integer
inv' a b = let (x, _, g) = euclidean a b in
  if abs g == 1 then x * g else error (show a ++ " and " ++ show b ++ " not relatively prime")

-- \a b -> let (x,y,g) = euclidean a b in abs g === gcd a b .&&. x * a + y * b === g
-- \a b -> abs a > 1 ==> let (_,y,_) = euclidean a b in abs y < abs a
-- \a b -> abs b > 1 ==> let (x,_,_) = euclidean a b in abs x < abs b
euclidean :: Integer -> Integer -> (Integer, Integer, Integer)
euclidean a b = go (1, 0, a) (0, 1, b)
  where
    go t (_, _, 0) = t
    go (x0, y0, r0) (x1, y1, r1) = go (x1, y1, r1) (x0 - m * x1, y0 - m * y1, r2)
      where
        (m, r2) = r0 `divMod` r1
