{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module GenericDemo where

import Keelung
import MerkleTree
import GHC.Generics

data Enc a = A a | R (Enc a) a a
  deriving Generic

instance Encode a => (Encode (Enc a))

testEnc :: Enc a -> Comp (Enc a)
testEnc = return