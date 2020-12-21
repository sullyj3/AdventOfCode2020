{-# LANGUAGE BlockArguments #-}

module Day14Spec where


import Test.Hspec
--import Control.Exception (evaluate)

import Text.Megaparsec
import qualified Data.Set            as S
import           Data.Set            (Set)
import Day14

spec = do
  let input = unlines [ "mask = 000000000000000000000000000000X1001X"
                      , "mem[42] = 100"
                      , "mask = 00000000000000000000000000000000X0XX"
                      , "mem[26] = 1"]
      parsed = parse parseProg "day14test2.txt" input
      Right parsed' = parsed
  describe "parseProg" do
    it "works for the example" do
      let expected = Right [ UpdateMask "000000000000000000000000000000X1001X" 
                     , MemWrite 42 100
                     , UpdateMask "00000000000000000000000000000000X0XX"
                     , MemWrite 26 1]
      parsed `shouldBe` expected


  describe "allAddrs" do
    it "works for the example" do
      let expected = S.fromList [ 26, 27, 58, 59 ]
          actual = allAddrs "000000000000000000000000000000X1001X" 42

      actual `shouldBe` expected

  describe "part2" do
    it "works for the example" do
      part2 parsed' `shouldBe` 208
