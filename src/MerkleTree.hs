{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module MerkleTree where

import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Hash.Poseidon
import Keelung

mkTree :: [Field] -> Comp Field
mkTree xs = do
  nodes <- mkNodes xs
  case nodes of
    [x] -> return x
    xs' -> mkTree xs'
  where
    mkNodes :: [Field] -> Comp [Field]
    mkNodes xs' = do
      node <- hash $ take 5 xs'
      rest <- case drop 5 xs' of
        [] -> return []
        xs'' -> mkNodes xs''
      return $ node : rest

-- Return the root as proof
getMerkleProof :: Field -> [[Field]] -> [Field] -> Comp Field
getMerkleProof leaf siblings indices = do
  foldlM
    ( \_digest (_i, p) -> do
        hash p >>= reuse
    )
    leaf
    (zip indices siblings)

getMerkleProof' :: Int -> Comp Field
getMerkleProof' depth = do
  leaf <- inputField Private
  siblings <- inputList2 Private depth 2
  indices <- inputList Private depth :: Comp [Field]
  foldlM
    (\_digest (_i, p) -> hash p >>= reuse)
    leaf
    (zip indices siblings)

choose :: [Field] -> Field -> Field
choose [] _ = 0
choose (x : xs) i = cond (i `eq` (4 - Integer (fromIntegral $ length xs))) x $ choose xs i

-- Custom datatype version making use of datatype-generic programming
data Tree a = Node a (Tree a) (Tree a) | Leaf a
  deriving (Generic)

instance (Encode a) => (Encode (Tree a))

type MerkleTree = Tree Field

mkBTree' :: Comp MerkleTree
mkBTree' = do
  xs <- inputList Private 5
  mkBTree xs

mkBTree :: [Field] -> Comp MerkleTree
mkBTree xs = do
  nodes <- mergeTrees (map Leaf xs, [])
  return $ fromMaybe (error "Empty Tree") nodes
  where
    -- Input:  "Unprocessed" trees and "Processed" trees
    mergeTrees :: ([MerkleTree], [MerkleTree]) -> Comp (Maybe MerkleTree)
    mergeTrees ([], []) = return Nothing
    mergeTrees ([t], []) = return $ Just t
    mergeTrees ([t], ts) = mergeTrees ([], t : ts)
    mergeTrees (t1 : t2 : ts', ts) = hash (map getRoot [t1, t2]) >>= \t -> mergeTrees (ts', Node t t1 t2 : ts)
    mergeTrees ([], [t]) = return $ Just t
    mergeTrees ([], ts) = mergeTrees (ts, [])

getRoot :: Tree a -> a
getRoot (Node n _ _) = n
getRoot (Leaf n) = n

-- data TaggedPair a = Fst a a | Snd a a
--
-- type Path = [TaggedPair Field]
--
-- dfs :: MerkleTree -> Comp (Maybe Path)
-- dfs tree = do
--   leaf <- inputField Private
--   return $ dfs' tree leaf
--   where
--     dfs' :: MerkleTree -> Field -> Maybe Path
--     dfs' (Node t1 t2 node) n =
--       if node == n
--         then Just []
--         else case (dfs' t1 n, dfs' t2 n) of
--           (Nothing, Nothing) -> Nothing
--           (Just p, _) -> Just (Fst (getRoot t1) (getRoot t2) : p)
--           (_, Just p) -> Just (Snd (getRoot t1) (getRoot t2) : p)
--     dfs' (Leaf l) n = if l == n then Just [] else Nothing
