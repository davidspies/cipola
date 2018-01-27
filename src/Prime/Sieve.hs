module Prime.Sieve
    ( fmergeAll
    , primes
    ) where

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x : xs') ys@(y : ys')
  | y < x = y : merge xs ys'
  | otherwise = x : merge xs' ys

fmerge :: Ord a => [a] -> [a] -> [a]
fmerge [] ys       = ys
fmerge (x : xs) ys = x : merge xs ys

fmergePrefix :: Ord a => Int -> [[a]] -> ([a], [[a]])
fmergePrefix _ []       = ([], [])
fmergePrefix 1 (x : xs) = (x, xs)
fmergePrefix n xs = (fmerge l r, rest)
  where
    halfN = n `quot` 2
    (l, mid) = fmergePrefix halfN xs
    (r, rest) = fmergePrefix (n - halfN) mid

fmergeAll :: Ord a => [[a]] -> [a]
fmergeAll = go 1
  where
    go _ [] = []
    go n xs = fmerge xs' (go (n * 2) rest)
      where
        (xs', rest) = fmergePrefix n xs

exclude :: Ord a => [a] -> [a] -> [a]
exclude [] _ = []
exclude xs [] = xs
exclude xs@(x : xs') ys@(y : ys') = case compare x y of
  LT -> x : exclude xs' ys
  EQ -> exclude xs' ys'
  GT -> exclude xs ys'

oddPrimes :: [Integer]
oddPrimes = 3 : exclude [5,7..] oddComposites

oddComposites :: [Integer]
oddComposites = fmergeAll [[p * p, p * (p + 2)..] | p <- oddPrimes]

primes :: [Integer]
primes = 2 : oddPrimes
