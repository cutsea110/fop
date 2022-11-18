-- ref.) https://arxiv.org/pdf/2110.01111.pdf
module ICBICSort (sort) where

import Debug.Trace (trace)

debug = True

($?) :: Show a => (a -> b) -> a -> b
f $? x = if debug then trace (show x) (f x) else f x

{- | I Can't Believe It Can Sort Algorithm.
>>> sort [1,3,2,5,4,7,6,9]
([],[1,3,2,5,4,7,6,0])
([7],[1,2,3,4,5,6,0])
([1,7],[2,3,4,5,6,0])
([1,2,7],[3,4,5,6,0])
([1,2,3,7],[4,5,6,0])
([1,2,3,4,7],[5,6,0])
([1,2,3,4,5,7],[6,0])
([1,2,3,4,5,6,7],[0])
([0,1,2,3,4,5,6,7],[])
[0,1,2,3,4,5,6,7]
-}
sort :: (Show a, Ord a) => [a] -> [a]
sort xs = swapper $? ([], xs)

swapper :: (Show a, Ord a) => ([a], [a]) -> [a]
swapper (xs,   []) = xs
swapper (xs, y:ys) = swapper $? (zs++[w], ws)
  where (z, zs) = swap (y, xs)
        (w, ws) = swap (z, ys)

swap :: Ord a => (a, [a]) -> (a, [a])
swap (x, xs) = case break (x<) xs of
  (xs',   []) -> (x, xs')
  (xs', y:ys) -> f (x, xs') $ swap (y, ys)
    where f (x, xs) (y, ys) = (y, xs++[x]++ys)
