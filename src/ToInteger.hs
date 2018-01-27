module ToInteger
    ( ToInteger(..)
    ) where

import Prelude as P

class ToInteger n where
  toInteger :: n -> Integer
instance ToInteger Int where
  toInteger = P.toInteger
