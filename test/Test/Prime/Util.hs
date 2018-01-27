{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Prime.Util() where

import Control.DeepSeq (NFData (..))
import Data.Maybe (fromMaybe)
import Prelude hiding (toInteger)
import Prime
import Test.QuickCheck
import ToInteger (toInteger)

instance Arbitrary Prime where
  arbitrary = do
    Positive n <- arbitrary
    let dp = downPrime n
        up = upPrime n
    case dp of
      Nothing  -> return up
      Just dp' -> return $
        if toInteger up - n < n - toInteger dp' then up else dp'

instance NFData Prime where
  rnf = rnf . toInteger

downPrime :: Integer -> Maybe Prime
downPrime n
  | n <= 1 = Nothing
  | otherwise = case mkPrime n of
      Nothing -> downPrime (n - 1)
      Just p  -> Just p

upPrime :: Integer -> Prime
upPrime n
  | n <= 1 = upPrime 2
  | otherwise = fromMaybe (upPrime (n + 1)) (mkPrime n)
