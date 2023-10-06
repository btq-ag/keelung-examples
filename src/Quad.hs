{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DataKinds #-}

{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Quad where

-- import Control.Monad
import Keelung
 

-- Example provided by Kohei
-- | Take three public inputs a, b, c, and a private input x,
--   asserts that `a * x * x + b * x + c = 0`.
--   E.g. `ghci> prove gf181 quad [3,5,-22] [2]`
quad :: Comp ()
quad = do
    _a :: Field <- input Public
    _b :: Field <- input Public
    _c :: Field <- input Public
    _x :: Field <- input Private
    assert (eq (_a * _x * _x + _b * _x + _c) 0)
    return ()
