{-# LANGUAGE BlockArguments #-}

module Day15Spec where


import Test.Hspec
--import Control.Exception (evaluate)

import Data.List (intersperse)
import Day15

spec = do
  let testInput = "0,3,6"
      Just parsed = parse testInput

  describe "parse" $ do
    it "works on the example" $ do
      parsed `shouldBe` [0,3,6]

  describe "part1" $ do
    it "works on the example" $ do
      part1 parsed `shouldBe` 0


