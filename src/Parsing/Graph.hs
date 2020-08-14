module Parsing.Graph where

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.List as List
import Data.Maybe(fromMaybe)
import Control.Monad.Trans.State.Strict
import Static.UnionFind(UnionFind)
import qualified Static.UnionFind as UF
import Common

data Graph a =
    MkGraph { getChildMap :: Map a (Set a) -- maps parents to children
            , getParentMap :: Map a (Set a) -- maps children to parents
            }
            deriving(Eq)

instance (Show a, Ord a) => Show (Graph a) where
    show g =
        getEdges g
        |> Set.toList
        |$> (\(a,b) -> unwords[show a,"->",show b])
        |> unlines

empty :: Graph a
empty = MkGraph Map.empty Map.empty

-- | given a map from key to set of value, ensure the given key maps to a set containing the given value
insertAdd :: (Ord k, Ord v) => k -> v -> Map k (Set v) -> Map k (Set v)
insertAdd k v m = case Map.lookup k m of
    Nothing -> Map.insert k (Set.singleton v) m
    Just vs -> Map.insert k (Set.insert v vs) m

-- | ensure that the given key is in the map. If it is not, add it with an empty set value
insertSafe :: (Ord k, Ord v) => k -> Map k (Set v) -> Map k (Set v)
insertSafe k m = case Map.lookup k m of
    Nothing -> Map.insert k Set.empty m
    Just{} -> m

fromList :: (Ord a) => [(a,a)] -> Graph a
fromList = foldr (uncurry addEdge) empty

addNode :: Ord a => a -> Graph a -> Graph a
addNode a g = g{getChildMap = insertSafe a (getChildMap g), getParentMap = insertSafe a (getParentMap g)}

addEdge :: Ord a => a -> a -> Graph a -> Graph a
addEdge parent child g = g{getChildMap = insertAdd parent child (getChildMap g), getParentMap = insertAdd child parent (getParentMap g)}

getParents :: Ord a => Graph a -> a -> Set a
getParents g child = fromMaybe Set.empty $ Map.lookup child (getChildMap g)

getChildren :: Ord a => Graph a -> a -> Set a
getChildren g parent = fromMaybe Set.empty $ Map.lookup parent (getParentMap g)

getNodes :: Ord a => Graph a -> Set a
getNodes g = Set.unions (Map.keysSet <$> [getParentMap g, getChildMap g])

getEdges :: Ord a => Graph a -> Set (a,a)
getEdges g =
    [getChildMap g]
    |$> Map.toList
    |> concat
    |> concatMap (\(x,xs) -> [(x,x') | x' <- Set.toList xs])
    |> Set.fromList

-- strongly connected components via Kosaraju's algorithm

visitVertex :: Ord a => Graph a -> a -> State (Set a, [a]) ()
visitVertex g v = do
    (visited, l) <- get
    if v `elem` visited
    then return ()
    else do put (Set.insert v visited, l) -- mark visited
            mapM_ (visitVertex g) (getChildren g v) -- dfs
            (visited', l') <- get
            put (visited', v:l') -- prepend node to l

assign :: Ord a => Graph a -> a -> a -> State (Set a, UnionFind a) ()
assign g v root = do
    (visited, uf) <- get
    if v `elem` visited
    then return ()
    else do put (Set.insert v visited, UF.union v root uf)
            mapM_ (\u -> assign g u root) (getParents g v)

coalesceWith :: (Ord a, Ord b) => (a -> b) -> Graph a -> Graph b
coalesceWith f g =
    let replaceEdge (a,b) = (f a, f b) in
    getEdges g
    |> Set.toList
    |$> replaceEdge
    |> fromList

coalesceSCCs :: Ord a => Graph a -> Graph (Set a)
coalesceSCCs g =
    let (_,(_,l)) = runState (mapM_ (visitVertex g) (getNodes g)) (Set.empty, [])
        (_,(_,uf)) = runState (mapM_ (\u -> assign g u u) l) (Set.empty, UF.empty)
        groups = fst <$> UF.group uf
        -- | maps an element to the set of nodes in its SCC
        replacement x = Set.fromList $ fromMaybe (error "dependency analysis error") (List.find (x `elem`) groups)
    in coalesceWith replacement g

-- topological sort

topologicalSort :: (Ord a, Show a) => Graph a -> [a]
topologicalSort g = snd (execState (mapM_ topologicalSortHelp (getNodes g)) (Set.empty, []))
    where
        topologicalSortHelp curr = do
            let parents = getParents g curr
            (visited, ans) <- get
            if curr `elem` visited then return ()
            else do
                put (Set.insert curr visited, ans)
                let go parent = do
                        (visited', _) <- get
                        if parent `elem` visited'
                        then return ()
                        else topologicalSortHelp parent
                mapM_ go parents
                (visited', ans') <- get
                put (visited', ans'++[curr])


