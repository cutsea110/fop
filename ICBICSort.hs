-- ref.) https://arxiv.org/pdf/2110.01111.pdf
module ICBICSort (sort) where

import Debug.Trace (trace)

($?) :: Show a => (a -> b) -> a -> b
f $? x = trace (show x) (f x)

{- | I Can't Believe It Can Sort Algorithm.
>>> sort [1,3,2,5,4,7,6,9]
([],[1,3,2,5,4,7,6,9])
([9],[1,2,3,4,5,6,7])
([1,9],[2,3,4,5,6,7])
([1,2,9],[3,4,5,6,7])
([1,2,3,9],[4,5,6,7])
([1,2,3,4,9],[5,6,7])
([1,2,3,4,5,9],[6,7])
([1,2,3,4,5,6,9],[7])
([1,2,3,4,5,6,7,9],[])
[1,2,3,4,5,6,7,9]
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
  (xs', y:ys) -> (z, xs'++[x]++zs)
    where (z, zs) = swap (y, ys)
