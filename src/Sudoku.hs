{-# LANGUAGE DataKinds #-}
module Sudoku where

import Control.Monad
import Keelung

sudoku :: Comp ()
sudoku = do
  -- each cell in the problem matrix ranges from 1 to 9, with 0 indicating an empty cell
  problem <- inputList2 Public 9 9 :: Comp [[UInt 4]]
  solution <- inputList2 Private 9 9

  -- check if the solution is part of the problem:
  --    if the problem cell is not empty, then the solution cell must be equal to the problem cell
  forM_ [0 .. 8] $ \i ->
    forM_ [0 .. 8] $ \j ->
      assert $
        cond
          (problem !! i !! j `eq` 0)
          true
          ((problem !! i !! j) `eq` (solution !! i !! j))

  -- check if all numbers in the problem matrix are in the range 0 to 9
  forM_ [0 .. 8] $ \i ->
    forM_ [0 .. 8] $ \j ->
      from0to9 (problem !! i !! j)

  -- check if each row of the solution is valid:
  forM_ [0 .. 8] $ \i ->
    valid $ solution !! i

  -- check if each column of the solution is valid:
  forM_ [0 .. 8] $ \j ->
    valid $ map (!! j) solution

  -- check if each 3x3 sub-matrix of the solution is valid:
  forM_ [0, 3, 6] $ \i ->
    forM_ [0, 3, 6] $ \j ->
      valid $
        [ solution !! (i + x) !! (j + y)
          | x <- [0 .. 2],
            y <- [0 .. 2]
        ]

-- | Check if a list contain the numbers 1 to 9 exactly once
valid :: [UInt 4] -> Comp ()
valid xs = do
  assert $ sum xs `eq` 45
  assert $ product xs `eq` 362880

from0to9 :: UInt 4 -> Comp ()
from0to9 x = assertLTE x 9
    -- x `eq` 0 .|. x `eq` 1 .|. x `eq` 2 .|. x `eq` 3 .|. x `eq` 4 .|. x `eq` 5 .|. x `eq` 6 .|. x `eq` 7 .|. x `eq` 8 .|. x `eq` 9

testProblem :: [Integer]
testProblem =
  mconcat
    [ [0, 0, 0, 2, 6, 0, 7, 0, 0],
      [6, 8, 0, 0, 7, 0, 0, 9, 0],
      [1, 9, 0, 0, 0, 4, 5, 0, 0],
      [8, 2, 0, 1, 0, 0, 0, 4, 0],
      [0, 0, 4, 6, 0, 2, 9, 0, 0],
      [0, 5, 0, 0, 0, 3, 0, 2, 8],
      [0, 0, 9, 3, 0, 0, 0, 7, 4],
      [0, 4, 0, 0, 5, 0, 0, 3, 6],
      [7, 0, 3, 0, 1, 8, 0, 0, 0]
    ]

testSolution :: [Integer]
testSolution =
  mconcat
    [ [4, 3, 5, 2, 6, 9, 7, 8, 1],
      [6, 8, 2, 5, 7, 1, 4, 9, 3],
      [1, 9, 7, 8, 3, 4, 5, 6, 2],
      [8, 2, 6, 1, 9, 5, 3, 4, 7],
      [3, 7, 4, 6, 8, 2, 9, 1, 5],
      [9, 5, 1, 7, 4, 3, 6, 2, 8],
      [5, 1, 9, 3, 2, 6, 8, 7, 4],
      [2, 4, 8, 9, 5, 7, 1, 3, 6],
      [7, 6, 3, 4, 1, 8, 2, 5, 8]
    ]