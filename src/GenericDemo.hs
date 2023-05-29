{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module GenericDemo where

import Keelung
import MerkleTree
import GHC.Generics

data FieldTree = Leaf Field | Node Field FieldTree FieldTree
  deriving Generic

checkTree :: Int -> Field -> Comp ()
checkTree depth root = do
  inputTree <- inputList Private depth
  root' <- mkTree inputTree
  assert (root `eq` root')