{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
module Tutorial where

import Control.Monad
import Keelung

-- | A program that outputs whatever number is given.
echo :: Comp Field
echo = do
  x <- input Private -- request for a private input and bind it to 'x'
  return x -- return 'x'

-- | A program that requests for 2 inputs and does nothing with them
useless :: Comp ()
useless = do
  _x <- inputField Public -- request for a public input and bind it to 'x'
  _y <- inputField Private -- request for a private input and bind it to 'y'
  return ()

-- | A program that expects the second input to be the square of the first input
square :: Comp ()
square = do
  x <- inputField Public -- request for a public input and bind it to 'x'
  y <- inputField Private -- request for a private input and bind it to 'y'
  assert ((x * x) `eq` y) -- assert that 'y' equals to 'x' squared

-- | A stupid calculator has only 2 operations: addition and multiplication.
--   It takes an Boolean operation flag and 2 inputs, and returns the result.
calculator :: Comp Field
calculator = do
  addOrMultiply <- inputBool Public
  x <- inputField Public
  y <- inputField Public

  return $
    cond
      addOrMultiply
      (x + y)
      (x * y)

-- | A program that asserts all 10 inputs to be 42
allBe42 :: Comp ()
allBe42 = do
  xs <- inputList Public 10 :: Comp [Field]
  -- access elements of `xs` with indices from 0 to 9
  forM_ [0 .. 9] $ \i -> do
    assert (xs !! i `eq` 42)
  -- access elements of `xs` directly
  forM_ xs $ \x -> do
    assert (x `eq` 42)

-- | A program that sums all the 10 inputs
summation :: Comp Field
summation = do
  xs <- inputList Public 10
  return $ sum xs

-- | Birthday voucher example
birthday :: Comp Boolean
birthday = do
  -- these inputs are private witnesses
  hiddenMonth <- inputField Private
  hiddenDate <- inputField Private
  -- these inputs are public inputs
  month <- input Public
  date <- input Public

  return $ (hiddenMonth `eq` month) `And` (hiddenDate `eq` date)

-- | A program that outputs the input to the 4th power (without computation reusing)
notReused :: Comp [Field]
notReused = do
  x <- input Public
  let y = x * x * x * x
  return [y, y]

-- | A program that outputs the input to the 4th power (with computation reusing)
reused :: Comp [Field]
reused = do
  x <- input Public
  y <- reuse $ x * x * x * x
  return [y, y]
