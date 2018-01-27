module Prime.Internal where

import ToInteger (ToInteger (..))

newtype Prime = Prime Integer
  deriving (Eq, Ord, Show)

instance ToInteger Prime where
  toInteger (Prime n) = n
