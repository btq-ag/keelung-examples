{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}

module LogicGates where

import Keelung

-- | AND gate: Gives an output of 1 if both inputs are 1, and 0 otherwise. 
-- | Truth Table:
-- | A | B | A AND B |
-- |---|---|---------|
-- | T | T |    T    |
-- | T | F |    F    |
-- | F | T |    F    |
-- | F | F |    F    |
myAND :: Comp Boolean
myAND = do
  a <- input Private
  b <- input Private
  return (a .&. b)

-- | NAND gate: Gives an output of 1 if both inputs aren't 1, and 0 otherwise. 
-- | Truth Table:
-- | A | B | A NAND B |
-- |---|---|----------|
-- | T | T |    F     |
-- | T | F |    T     |
-- | F | T |    T     |
-- | F | F |    T     |
myNAND :: Comp Boolean
myNAND = do
  a <- input Private
  b <- input Private
  return (complement (a .&. b))

-- | OR gate: Gives an output of 1 if either of the inputs are 1, and 0 otherwise.
-- | Truth Table:
-- | A | B | A OR B  |
-- |---|---|---------|
-- | T | T |    T    |
-- | T | F |    T    |
-- | F | T |    T    |
-- | F | F |    F    |
myOR :: Comp Boolean
myOR = do
  a <- input Private
  b <- input Private
  return (a .|. b)

-- | XOR gate: Gives an output of 1 if only one of the inputs is 1, and 0 otherwise.
-- | Truth Table:
-- | A | B | A XOR B |
-- |---|---|---------|
-- | T | T |    F    |
-- | T | F |    T    |
-- | F | T |    T    |
-- | F | F |    F    |
myXOR :: Comp Boolean
myXOR = do
  a <- input Private
  b <- input Private
  return (a .^. b)

-- | NOR gate: Gives an output of 1 if only one of the two inputs are 0, and 0 otherwise.
-- | Truth Table:
-- | A | B | A NOR B |
-- |---|---|---------|
-- | T | T |    F    |
-- | T | F |    F    |
-- | F | T |    F    |
-- | F | F |    T    |
myNOR :: Comp Boolean
myNOR = do
  a <- input Private
  b <- input Private
  return (complement (a .|. b))

-- | XNOR gate: Gives an output of 1 if the two inputs are the same, and 0 if they are different.
-- | Truth Table:
-- | A | B | A XNOR B|
-- |---|---|---------|
-- | T | T |    T    |
-- | T | F |    F    |
-- | F | T |    F    |
-- | F | F |    T    |
myXNOR :: Comp Boolean
myXNOR = do
  a <- input Private
  b <- input Private
  return (complement (a .^. b))

-- | NOT gate: Inverter, gives the 
-- | Truth Table:
-- | A | NOT A |
-- |---|-------|
-- | T |   F   |
-- | F |   T   |
myNOT :: Comp Boolean
myNOT = do
  a <- input Private
  return (complement a)
