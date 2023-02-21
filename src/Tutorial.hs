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

-- | A program that expects the second input
-- to be the square of the first input
square :: Comp ()
square = do
  x <- inputField Private -- request for a private input and bind it to 'x'
  y <- inputField Private -- request for a private input and bind it to 'y'
  assert ((x * x) `eq` y) -- assert that 'y' equals to 'x' squared

-- | A program that asserts all 10 inputs to be 42
allBe42 :: Comp ()
allBe42 = do
  xs <- inputList Private 10
  y <- inputField Private
  forM_ xs $ \x -> assert (x `eq` y)

-- | A program that sums all the 10 inputs
summation :: Comp Field
summation = do
  xs <- inputList Private 10
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
  x <- input Private
  let y = x * x * x * x
  return [y, y]

-- | A program that outputs the input to the 4th power (with computation reusing)
reused :: Comp [Field]
reused = do
  x <- input Private
  y <- reuse $ x * x * x * x
  return [y, y]
