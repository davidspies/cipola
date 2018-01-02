module Test.ModuloSpec(spec) where

import           Modulo          (crt)
import           Test.Hspec
import           Test.QuickCheck (arbitrary, choose, getSize, property,
                                  suchThat, (===))

spec :: Spec
spec = describe "crt" $
  it "should return Nothing when moduli can't be unified" $ property $ do
    s <- toInteger <$> getSize
    g <- choose (2, 2 + s)
    m <- choose (1, 1 + s)
    n <- choose (1, 1 + s)
    (a, b) <- arbitrary `suchThat` (\(a, b) -> a `mod` g /= b `mod` g)
    return $ crt [(a, m * g), (b, n * g)] === Nothing
