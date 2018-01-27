import Factorize (factorize)
import Prelude hiding (toInteger)
import System.Environment (getArgs)
import ToInteger (toInteger)

main :: IO ()
main = do
  [inp] <- map read <$> getArgs
  mapM_ (print . toInteger) (factorize inp)
