-- ref.) https://arxiv.org/pdf/2110.01111.pdf
module ICBICSort
  ( sort
  , sortBy
  ) where

import Debug.Trace (trace)

debug = True

($?) :: Show a => (a -> b) -> a -> b
f $? x = if debug then trace (show x) (f x) else f x

-- I Can't Believe It Can Sort Algorithm.
sort :: (Show a, Ord a) => [a] -> [a]
sort = sortBy (<)

sortBy :: (Show a, Ord a) => (a -> a -> Bool) -> [a] -> [a]
sortBy cmp xs = swapperBy cmp $? ([], xs)

swapperBy :: (Show a, Ord a) => (a -> a -> Bool) -> ([a], [a]) -> [a]
swapperBy cmp (xs,   []) = xs
swapperBy cmp (xs, y:ys) = swapperBy cmp $? (zs++[w], ws)
  where (z, zs) = swpBy cmp (y, xs)
        (w, ws) = swpBy cmp (z, ys)

swpBy :: Ord a => (a -> a -> Bool) -> (a, [a]) -> (a, [a])
swpBy cmp (x, xs) = case break (x `cmp`) xs of
  (xs', [])   -> (x, xs')
  (xs', y:ys) -> (z, xs'++[x]++zs)
    where (z, zs) = swpBy cmp (y, ys)

sample = [1,3,2,5,4,7,6,0]

-- | please check!
-- import Data.Function (on)
-- sortBy ((<) `on` fst) sample2
sample2 = [(3,0), (3,1), (1,2)]
