{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Modulo
    ( module X
    , inv
    ) where

import           Data.Reflection (Reifies (..))
import           Modulo.Internal as X
import           Prelude         hiding (toInteger)

inv :: Reifies s Modulo => E s -> E s
inv a = fromInteger $ inv' (toInteger a) (modulusOf a)

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
