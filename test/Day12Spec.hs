{-# LANGUAGE BlockArguments #-}

module Day12Spec where

import Test.Hspec
import Day12

spec = do
  describe "rotateAround0" $ do
    it "does not affect the origin" $ do
      rotateAround0 (0,0) `shouldBe` (0,0)

    it "makes a vector facing due east face due north" $ do
      rotateAround0 (10,0) `shouldBe` (0,10)

    it "works for another example" $ do
      rotateAround0 (-5,13) `shouldBe` (-13,5)
