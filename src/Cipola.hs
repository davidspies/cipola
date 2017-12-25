{-# LANGUAGE LambdaCase #-}

module Cipola
    ( cipola
    , Parity(..)
    , parity
    ) where

import           Control.Monad.Reader (Reader, asks, runReader, withReader)
import           ModRing
import           Prelude              hiding (Num (..), toInteger, (^))
import qualified Prelude              as P
import           Ring
import           SquareRing

data Parity = Even | Odd

parity :: Integer -> Parity
parity n
  | odd n = Odd
  | otherwise = Even

jacobi :: Integer -> Integer -> Integer
jacobi a b
  | even b || b < 0 = error "Bad input"
  | a < 0 || a >= b = jacobi (a `mod` b) b
  | a <= 1 = a
  | otherwise = case parity a of
      Even -> case parity (a `quot` 2) of
        Even -> jacobi (a `quot` 4) b
        Odd  -> jacobi (a `quot` 2) b P.* jacobi2 b
      Odd -> (if  a `rem` 4 == 3 && b `rem` 4 == 3 then (-1) else 1) P.* jacobi b a

jacobi2 :: Integer -> Integer
jacobi2 n = case n `rem` 8 of
  1 -> 1
  3 -> -1
  5 -> -1
  7 -> 1
  _ -> error "Bad input"

firstJustM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
firstJustM xs0 f = go xs0
  where
    go xs = case xs of
      [] -> return Nothing
      x : xr -> f x >>= \case
        Nothing -> go xr
        Just res -> return $ Just res

-- \a p -> isPrime p && p > 2 ==> case cipola a p of
--    Nothing -> jacobi a p /= 1
--    Just x -> x ^ 2 `mod` p == a
cipola :: Integer -> Integer -> Maybe Integer
cipola a p = toInteger <$> runReader (e a >>= cipolaRing) (ModRing p)

cipolaRing :: Elem ModRing -> Reader ModRing (Maybe (Elem ModRing))
cipolaRing a = do
  m <- (\m -> if m <= 1 || even m then error "Bad input" else m) <$> asks modulus
  r <- firstJustM [1 .. (m P.- 1)] $ \x -> do
    x' <- e x
    tsqr' <- do
      v <- x' * x'
      v - a
    return $ if jacobi (toInteger tsqr') m == -1
      then Just (x', tsqr')
      else Nothing
  v0 <- zero
  v1 <- one
  case r of
    Nothing -> return Nothing
    Just (x, tsqr') -> withReader (`SquareRing` tsqr') $ do
      let s = ES x v1
      ES result z <- s ^ ((m P.+ 1) `quot` 2)
      return $ if z == v0 then Just result else Nothing
