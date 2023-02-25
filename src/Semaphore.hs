module Semaphore where

import Hash.Poseidon
import Keelung
import MerkleTree
import Prelude

semaphore ::
  Field ->
  Field ->
  [[Field]] ->
  [Field] ->
  Field ->
  Field ->
  -- Public: root of merkle tree, hash of nullifiers
  Comp (Field, Field)
semaphore identityNullifier identityTrapdoor siblings indices signalHash externalNullifier = do
  secret <- hash [identityNullifier, identityTrapdoor]
  commitment <- hash [secret]
  nullifierHash <- hash [externalNullifier, identityNullifier]
  root <- getMerkleProof commitment siblings indices
  _signalHashSquared <- reuse $ signalHash * signalHash
  return (root, nullifierHash)

-- Check two things: the signal is valid (cast by someone in a group), and it is not double signaling
checkSignal :: Int -> Field -> [Field] -> Field -> Field -> Comp ()
checkSignal depth root nullifierMap signalHash externalNullifier = do
  identityNullifier <- inputField Private
  identityTrapdoor <- inputField Private
  siblings <- inputList2 Private depth 5
  indices <- inputList Private depth

  -- check the commitment is in the tree
  (root', nullifierHash) <- semaphore identityNullifier identityTrapdoor siblings indices signalHash externalNullifier

  assert (root `eq` root')

  -- check the signal has not been cast
  assert (Boolean (nullifierHash `notElem` nullifierMap))