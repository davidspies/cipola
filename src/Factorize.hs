module Factorize
  ( factorize,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM_, void)
import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import qualified Data.DList as DList
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Proxy (Proxy)
import Data.Reflection (Reifies, reify)
import EllipticCurve (EPOC, PointOnCurve (..), randomPoints, (|*?|))
import Modulo (Modulo (Modulo))
import Prime (Prime, mkPrime, primePowers)
import ToInteger (toInteger)
import Prelude hiding (toInteger)

pickBase :: Integer -> [Integer]
pickBase n =
  map (toInteger . fst) $ takeWhile ((< m) . toInteger) primePowers
  where
    p :: Double
    p = sqrt (fromInteger n)
    logp = log p
    alpha = sqrt (2 * logp / log logp)
    logm = logp / alpha
    m = max 100 (ceiling $ exp logm)

maybeLeft :: Either a b -> Maybe a
maybeLeft = \case
  Left l -> Just l
  Right _ -> Nothing

tryPOC :: Reifies s Modulo => [Integer] -> EPOC s -> Maybe Integer
tryPOC base pa = maybeLeft $ do
  (PointOnCurve p) <- pa
  foldM_ (flip (|*?|)) p base

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (<|>) Nothing

findAFactor :: Integer -> Integer
findAFactor n
  | even n = 2
  | otherwise = reify (Modulo n) $ \(_ :: Proxy s) ->
    fromJust $
      firstJust
        ( [tryPOC base p | p <- randomPoints :: [EPOC s]]
            `using` parBuffer 100 rdeepseq
        )
  where
    base = pickBase n

factorize :: Integer -> [Prime]
factorize = DList.toList . go
  where
    go n = case compare n 1 of
      LT -> error "Must be positive"
      EQ -> DList.empty
      GT -> case mkPrime n of
        Just p -> DList.singleton p
        Nothing -> let k = findAFactor n in go k <> go (n `quot` k)
