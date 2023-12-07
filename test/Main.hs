{-# LANGUAGE DataKinds #-}

module Main where

import Test.Hspec
import Test.Snarkjs as Snarkjs

main :: IO ()
main = hspec $ do
  describe "Snarkjs" Snarkjs.testQuad
