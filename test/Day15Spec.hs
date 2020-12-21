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

  describe "theSequence" $ do
    it "works on the example" $ do
      take 10 (theSequence parsed) `shouldBe` [0,3,6,0,3,3,1,0,4,0]

  describe "part1" $ do
    it "works on the first example" $ do
      part1 parsed `shouldBe` 436

    it "works on the other examples" $ do
      let examples =
            [ ([1,3,2], 1)
            , ([2,1,3], 10)
            , ([1,2,3], 27)
            , ([2,3,1], 78)
            , ([3,2,1], 438)
            , ([3,1,2], 1836)
            ]

          (ins, outs) = unzip examples
      map part1 ins `shouldBe` outs


