{-# LANGUAGE BlockArguments #-}

module Day14Spec where


import Test.Hspec
--import Control.Exception (evaluate)

import qualified Data.Set            as S
import           Data.Set            (Set)
import Day14

spec = do
  describe "allAddrs" do
    it "works for the example" do
      let expected = S.fromList [ 26, 27, 58, 59 ]
          actual = allAddrs "000000000000000000000000000000X1001X" 42

      actual `shouldBe` expected
