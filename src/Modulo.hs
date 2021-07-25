module Modulo
  ( module X,
    crt,
    inv,
    tryInv,
  )
where

import Data.Foldable (foldl')
import Data.Reflection (Reifies (..))
import Modulo.Internal as X
import ToInteger (toInteger)
import Prelude hiding (toInteger)

inv :: Reifies s Modulo => E s -> E s
inv a = case inv' a' n of
  Left g -> error (show a' ++ " and " ++ show n ++ " share factor " ++ show g)
  Right v -> fromInteger v
  where
    a' = toInteger a
    n = modulusOf a

tryInv :: Reifies s Modulo => E s -> Either Integer (E s)
tryInv a = fromInteger <$> inv' (toInteger a) (modulusOf a)

inv' :: Integer -> Integer -> Either Integer Integer
inv' a b =
  let (x, _, g) = euclidean a b
   in if abs g == 1
        then Right (x * g)
        else Left g

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

crt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
crt =
  foldl'
    ( \case
        Nothing -> const Nothing
        Just (x1, m1) -> \(x2, m2) -> crt2 (x1, m1) (x2, m2)
    )
    (Just (0, 1))

crt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
crt2 (a1, m1) (a2, m2)
  | r == a2 `mod` g = Just ((a1 `div` g * y * m2 + a2 `div` g * x * m1) `mod` m + r, m)
  | otherwise = Nothing
  where
    (x, y, g) = euclidean m1 m2
    m = m1 * m2 `div` g
    r = a1 `mod` g
