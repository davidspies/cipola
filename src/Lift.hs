module Lift where

import           Control.Monad.Reader (runReader)
import           Prelude              hiding (Num (..))
import qualified Prelude              as P

import           ModRing
import           Ring                 hiding ((^))

liftSol :: Integer -> Int -> Integer -> Integer -> Integer
liftSol prime curPow val sol = mult P.* (prime P.^ curPow) P.+ sol
  where
    curMod = prime ^ curPow
    k = val `div` curMod
    j = sol P.* sol `div` curMod
    mult = (`runReader` ModRing prime) $ do
      n <- e (k P.- j)
      twoSolInv <- inv =<< e (2 P.* sol)
      unEM <$> n * twoSolInv
