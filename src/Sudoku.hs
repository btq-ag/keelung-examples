module Sudoku where

import Control.Monad
import Keelung

sudoku :: Comp ()
sudoku = do
  -- each cell in the problem matrix ranges from 1 to 9, with 0 indicating an empty cell
  problem <- inputList2 Public 9 9 :: Comp [[Field]]
  soluton <- inputList2 Private 9 9

  -- check if all numbers in the solution matrix are in the range 1 to 9
  forM_ [0 .. 8] $ \i ->
    forM_ [0 .. 8] $ \j ->
      assert $ from1to9 (problem !! i !! j)

  -- check if the solution is part of the problem:
  --    if the problem cell is not empty, then the solution cell must be equal to the problem cell
  forM_ [0 .. 8] $ \i ->
    forM_ [0 .. 8] $ \j ->
      when (problem !! i !! j /= 0) $
        assert $
          (soluton !! i !! j) `eq` (problem !! i !! j)

  -- check if each row of the solution is valid:
  forM_ [0 .. 8] $ \i ->
    valid $ soluton !! i

  -- check if each column of the solution is valid:
  forM_ [0 .. 8] $ \j ->
    valid $ map (!! j) soluton

  -- check if each 3x3 sub-matrix of the solution is valid:
  forM_ [0, 3, 6] $ \i ->
    forM_ [0, 3, 6] $ \j ->
      valid $
        [ soluton !! (i + x) !! (j + y)
          | x <- [0 .. 2],
            y <- [0 .. 2]
        ]

-- | Check if a list contain the numbers 1 to 9 exactly once
valid :: [Field] -> Comp ()
valid xs = do
  assert $ sum xs `eq` 45
  assert $ sum xs `eq` 362880

from1to9 :: Field -> Boolean
from1to9 x = x `eq` 1 .|. x `eq` 2 .|. x `eq` 3 .|. x `eq` 4 .|. x `eq` 5 .|. x `eq` 6 .|. x `eq` 7 .|. x `eq` 8 .|. x `eq` 9