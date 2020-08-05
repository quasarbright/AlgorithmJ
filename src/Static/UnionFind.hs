module Static.UnionFind(UnionFind(), empty, union, find, toRep) where

import qualified Data.Map as Map
import qualified Data.List as List

type UnionFind a = (Map.Map a a)

empty :: UnionFind a
empty = Map.empty

union :: Ord a => a -> a -> UnionFind a -> UnionFind a
union a b uf =
    let (a', uf') = find' uf a
        (b', uf'') = find' uf' b
        rep = max a' b'
    in foldr (`Map.insert` rep) uf'' (List.nub [a,b,a',b'])

find :: Ord a => UnionFind a -> a -> a
find uf a = case Map.lookup a uf of
    Nothing -> a
    Just a'
        | a == a' -> a
        | otherwise -> find uf a'

find' :: Ord a => UnionFind a -> a -> (a, UnionFind a)
find' uf a =
    let as = toRep uf a
        rep = last as
    in (rep, foldr (`Map.insert` rep) uf as)

--compact :: Ord a => UnionFind a -> UnionFind a
--compact uf = foldr smartAdd empty (Map.keys uf)
--    where smartAdd a uf' = union a (find uf a) uf'

toRep :: Ord a => UnionFind a -> a -> [a]
toRep uf a = case Map.lookup a uf of
    Nothing -> [a]
    Just a'
        | a == a' -> [a]
        | otherwise -> a:a':toRep uf a'
