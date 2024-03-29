module Hylo where

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)

fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f g = u
  where
    u (Leaf x) = f x
    u (Node ts) = g (map u ts)

para :: (a -> b) -> ([(Tree a, b)] -> b) -> Tree a -> b
para g h = u
  where
    (f1 &&& f2) x = (f1 x, f2 x)
    u (Leaf x) = g x
    u (Node ts) = h (map (id &&& u) ts)

leaf :: a -> Tree a
leaf = Leaf
node :: [Tree a] -> Tree a
node = Node
  
unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x = if p x then Leaf (v x)
                 else Node (map (unfold p v h) (h x))

data LTree a = LLeaf a | LNode a [LTree a] deriving (Show, Eq)

fill :: (a -> b) -> ([b] -> b) -> Tree a -> LTree b
fill f g = fold (lleaf f) (lnode g)
-- fill f g = fold (LLeaf . f) (LNode <$> g . map label <*> id)

lleaf :: (t -> a) -> t -> LTree a
lleaf f x = LLeaf (f x)
lnode :: ([a] -> a) -> [LTree a] -> LTree a
lnode g ts = LNode (g (map label ts)) ts

label :: LTree a -> a
label (LLeaf x) = x
label (LNode x ts) = x

hylo :: ([a] -> b) -> ([b] -> b) -> ([a] -> [[a]]) -> [a] -> b
hylo f g h = fold f g . mkTree h

mkTree :: ([a] -> [[a]]) -> [a] -> Tree [a]
mkTree h = unfold single id h

single [x] = True
single _ = False

-- map f = (| α . F (f, id) |) = (|[Leaf, Node] . (f + id)|) = (|Leaf . f, Node|)
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = fold (eta . f) Node


-- F_A(B) = A + G(B) <= ( G(B) = [] B )
-- φ = α . inl = [Leaf, Node] . inl = Leaf
eta :: a -> Tree a
eta = Leaf
-- ψ = (|id, α . inr|) = (|id, [Leaf, Node] . inr|) = (|id, Node|)
mu :: Tree (Tree a) -> Tree a
mu = fold id Node

instance Functor Tree where
  -- fmap = (|Leaf . f, Node|) = mapTree
  fmap f = fold (Leaf . f) Node

instance Applicative Tree where
  pure = eta
  -- (<*> tra) = fold (<$> tra) Node
  fs <*> tra = fold (<$> tra) Node fs

instance Monad Tree where
  m >>= f = mu (f <$> m)


----------------------------------------------------------

split :: [a] -> [[a]]
split xs = [take n xs, drop n xs] where n = length xs `div` 2

----------------------------------------------------------

{-# INLINE[1] isegs #-}
isegs :: [a] -> [[a]]
isegs xs = [init xs, tail xs]

{-# INLINE[1] recover #-}
recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

{-# RULES
  "recover/isegs" forall xs. recover (isegs xs) = xs
  #-}

----------------------------------------------------------

minors :: [a] -> [[a]]
minors [x,y] = [[x],[y]]
minors (x:xs) = map (x:) (minors xs) ++ [xs]

----------------------------------------------------------

mkNexus :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus f g = label . extractL . until singleL (stepL g) . initialL f
-- mkNexus f g = label . fill f g . mkTree minors

initialL :: ([a] -> b) -> [a] -> Layer (LTree b)
stepL :: ([b] -> b) -> Layer (LTree b) -> Layer (LTree b)
singleL :: Layer (LTree b) -> Bool
extractL :: Layer (LTree b) -> LTree b

-- common

-- type Layer a = [a]

wrap :: a -> [a]
wrap = (:[])

-- h = split
{-
group :: [a] -> [[a]]
group [] = []
group (x:y:xs) = [x,y]:group xs

initialL f = map (lleaf f . wrap)
stepL g = map (lnode g) . group
singleL = single
extractL = head
-}
-- h = isegs
{-
group :: [a] -> [[a]]
group [x] = []
group (x:y:xs) = [x,y]:group (y:xs)
-}

-- h = minors

type Layer a = [Tree a]

{-
group :: [a] -> [[a]]
group [x] = []
group (x:xs) = map (bind x) xs ++ group xs
  where bind x y = [x,y]
-}

group :: [Tree a] -> [Tree [a]]
group [t] = []
group (Leaf x:vs) = Node [Leaf [x,y] | Leaf y <- vs]:group vs
group (Node us:vs) = Node (zipWith combine (group us) vs):group vs

combine :: Tree [a] -> Tree a -> Tree [a]
combine (Leaf xs) (Leaf x) = Leaf (xs ++ [x])
combine (Node us) (Node vs) = Node (zipWith combine us vs)

initialL f = map (Leaf . lleaf f . wrap)
singleL = single
extractL = extract . head
  where
    extract = para id (snd . head)
stepL g = map (mapTree (lnode g)) . group

uncats :: [a] -> [([a],[a])]
uncats [x,y] = [([x], [y])]
uncats (x:xs) = ([x], xs) : map (cons x) (uncats xs)
  where cons x (ys, zs) = (x:ys, zs)

ex1 = mkNexus id concat "abcd"
ex2 = mkNexus id concat "abcde"
