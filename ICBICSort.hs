-- ref.) https://arxiv.org/pdf/2110.01111.pdf
module ICBICSort
  ( sort
  ) where

import Debug.Trace (trace)

debug = True

($?) :: Show a => (a -> b) -> a -> b
f $? x = if debug then trace (show x) (f x) else f x

-- I Can't Believe It Can Sort Algorithm.
sort :: (Show a, Ord a) => [a] -> [a]
sort xs = swapper $? ([], xs)

swapper :: (Show a, Ord a) => ([a], [a]) -> [a]
swapper (xs,   []) = xs
swapper (xs, y:ys) = swapper $? (zs++[w], ws)
  where (z, zs) = swap (y, xs)
        (w, ws) = swap (z, ys)

swap :: Ord a => (a, [a]) -> (a, [a])
swap (x, xs) = case break (x<) xs of
  (xs', [])   -> (x, xs')
  (xs', y:ys) -> f (x, xs') $ swap (y, ys)
    where f (x, xs) (y, ys) = (y, xs++[x]++ys)

sample = [1,3,2,5,4,7,6,0]
