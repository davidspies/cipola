{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module SquareRing where

import           Control.Monad        (join)
import           Control.Monad.Reader (ask, withReader)
import           ModRing
import           Prelude              hiding (Num (..))
import           Ring

data SquareRing = SquareRing {base :: ModRing, tsqr :: Elem ModRing}

instance Ring SquareRing where
  data Elem SquareRing = ES {constant :: Elem ModRing, tcoef :: Elem ModRing}
    deriving (Eq, Show)
  zero = withReader base $ ES <$> zero <*> zero
  one = withReader base $ ES <$> one <*> zero
  negate ES{constant, tcoef} = withReader base $
    ES <$> negate constant <*> negate tcoef
  (+) (ES cl tl) (ES cr tr) = withReader base $ ES <$> cl + cr <*> tl + tr
  (*) (ES cl tl) (ES cr tr) = do
    SquareRing{tsqr} <- ask
    withReader base $ do
      left <- join $ (+) <$> (cl * cr) <*> join ((* tsqr) <$> (tl * tr))
      right <- join $ (+) <$> (cl * tr) <*> (tl * cr)
      return $ ES left right
  (-) (ES cl tl) (ES cr tr) = withReader base $ ES <$> cl - cr <*> tl - tr
  e n = withReader base $ ES <$> e n <*> zero
