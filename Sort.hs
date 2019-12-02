{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | A Duality of Sorts.
--   by Ralf Henze.
--   http://dreixel.net/research/pdf/ds.pdf
-- 
module Sort where

import Control.Monad (replicateM, forM_)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Function (on)
import qualified Data.Foldable as Foldable
import Data.List (unfoldr, foldl', sort, (\\), delete, nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import System.IO (hPutStr, hPutStrLn, stdin, stdout, withFile, IOMode(..))

-- swap (x, y) = (y, x)
pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

newtype Fix f = In { out :: f (Fix f) }

data Hisx f a x = Hisx { unHisx :: (a, f x) } deriving (Show, Functor)
newtype Cofree f a = Cf { unCf :: Fix (Hisx f a) }
instance Functor f => Functor (Cofree f) where
  fmap f = Cf . ana (phi . out) . unCf
    where phi (Hisx (a, x)) = Hisx (f a, x)

extract :: Functor f => Cofree f t -> t
extract cf = case out (unCf cf) of
  Hisx (a, _) -> a

sub :: Functor f => Cofree f a -> f (Cofree f a)
sub cf = case out (unCf cf) of
  Hisx (_, b) -> fmap Cf b

data Futx f a x = Futx { unFutx :: Either a (f x) } deriving (Show, Functor)
newtype Free f a = Fr { unFr :: Fix (Futx f a) }
instance Functor f => Functor (Free f) where
  fmap f = Fr . cata (In . phi) . unFr
    where phi (Futx (Left a)) = Futx (Left (f a))
          phi (Futx (Right x)) = Futx (Right x)

inject :: a -> Free f a
inject = Fr . In . Futx . Left

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = phi . fmap (hylo phi psi) . psi -- cata phi . ana psi
-- metamorphism
meta :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta phi psi = In . fmap (meta phi psi) . out -- ana psi . cata phi
-- paramorphism
para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para phi = phi . fmap (pair (id, para phi)) . out
-- apomorphism
apo :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo psi = In . fmap (uncurry either (id, apo psi)) . psi
-- histomorphism
histo :: Functor f => (f (Cofree f t) -> t) -> Fix f -> t
histo phi = extract . cata ap
  where ap = cast . Hisx . pair (phi, id)
        cast = Cf . In . fmap unCf
-- futumorphism
futu :: Functor f => (t -> f (Free f t)) -> t -> Fix f
futu psi = ana ap . inject
  where ap = uncurry either (psi, id) . unFutx . cast
        cast = fmap Fr . out . unFr
-- chronomorphism
chrono :: Functor f => (f (Cofree f b) -> b) -> (a -> f (Free f a)) -> a -> b
chrono phi psi = extract . hylo phi' psi' . inject
  where
    phi' = toCofree . Hisx . pair (phi, id)
    toCofree = Cf . In . fmap unCf
    psi' = uncurry either (psi, id) . unFutx . fromFree
    fromFree = fmap Fr . out . unFr
-- cochronomorphism
cochrono :: Functor f => (f (Cofree f t) -> t) -> (t -> f (Free f t)) -> Fix f -> Fix f
cochrono phi psi = futu psi . histo phi
-- zygomorphism
zygo :: Functor f => (f a -> a) -> (f (a, b) -> b) -> Fix f -> b
zygo f phi = snd . cata (pair (f . fmap fst, phi))
-- cozygomorphism
cozygo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> b -> Fix f
cozygo f psi = ana (uncurry either (fmap Left . f, psi)) . Right
-- dynamorphism
dyna :: Functor f => (f (Cofree f b) -> b) -> (a -> f a) -> a -> b
dyna f g = chrono f (fmap inject . g) -- histo f . ana g
-- codynamorphism
codyna :: Functor f => (f b -> b) -> (a -> f (Free f a)) -> a -> b
codyna f g = chrono (f . fmap extract) g
-- mutumorphism
mutu :: Functor f => (a -> b) -> (f a -> a) -> Fix f -> b
mutu proj phi = proj . cata phi
-- comutumorphism
comutu :: Functor f => (b -> a) -> (a -> f a) -> b -> Fix f
comutu proj psi = ana psi . proj


foldn (c, f) 0 = c
foldn (c, f) n = f (foldn (c, f) (n-1))

paran (c, f) 0 = c
paran (c, f) n = f n (paran (c, f) (n-1))

------

data ListF a r = Nil | Cons a r deriving (Show, Functor)
type List a = Fix (ListF a)

nil :: List a
nil = In Nil

cons :: a -> List a -> List a
cons x xs = In (Cons x xs)

instance Show a => Show (List a) where
  show (In Nil) = "Nil"
  show (In (Cons x xs)) = "(Cons " ++ show x ++ " " ++ show xs ++ ")"
  

data SListF a r = SNil | SCons a r deriving (Show, Functor)
type SList a = Fix (SListF a)

snil :: SList a
snil = In SNil

scons :: a -> SList a -> SList a
scons x xs = In (SCons x xs)

instance Show a => Show (SList a) where
  show (In SNil) = "SNil"
  show (In (SCons x xs)) = "(SCons " ++ show x ++ " " ++ show xs ++ ")"

----------------------------------------------------------------------------
-- specialize
type ListF' = ListF Int
type SListF' = SListF Int

swap :: ListF' (SListF' a) -> SListF' (ListF' a)
swap Nil = SNil
swap (Cons a SNil) = SCons a Nil
swap (Cons a (SCons b x))
  | a <= b         = SCons a (Cons b x)
  | otherwise      = SCons b (Cons a x)

naiveInsertSort' :: Fix (ListF Int) -> Fix (SListF Int)
naiveInsertSort' = cata (ana (swap . fmap out))

bubbleSort' :: Fix (ListF Int) -> Fix (SListF Int)
bubbleSort' = ana (cata (fmap In . swap))

swop :: ListF' (a, SListF' a) -> SListF' (Either a (ListF' a))
swop Nil = SNil
swop (Cons a (x, SNil)) = SCons a (Left x)
swop (Cons a (x, SCons b x'))
  | a <= b              = SCons a (Left x)
  | otherwise           = SCons b (Right (Cons a x'))

insertSort' :: Fix (ListF Int) -> Fix (SListF Int)
insertSort' = cata (apo (swop . fmap (pair (id, out))))

selectSort' :: Fix (ListF Int) -> Fix (SListF Int)
selectSort' = ana (para (fmap (either id In) . swop))

-- Utilities
toList :: [a] -> List a
toList = foldr cons nil

fromList :: List a -> [a]
fromList = unfoldr psi
  where
    psi (In Nil) = Nothing
    psi (In (Cons x xs)) = Just (x, xs)

fromSList :: SList a -> [a]
fromSList = unfoldr psi
  where
    psi (In SNil) = Nothing
    psi (In (SCons x xs)) = Just (x, xs)

sortBy :: Ord a => (List a -> SList a) -> [a] -> [a]
sortBy sorter = fromSList . sorter . toList

naiveInsertSort, bubbleSort, insertSort, selectSort :: [Int] -> [Int]
naiveInsertSort = sortBy naiveInsertSort'
bubbleSort = sortBy bubbleSort'
insertSort = sortBy insertSort'
selectSort = sortBy selectSort'
