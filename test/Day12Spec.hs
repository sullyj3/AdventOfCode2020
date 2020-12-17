{-# LANGUAGE BlockArguments #-}

module Day12Spec where

import Test.Hspec
import Day12

import Data.List (scanl')

spec = do
  describe "rotate90CCwAround0" $ do
    it "does not affect the origin" $ do
      rotate90CCwAround0 (0,0) `shouldBe` (0,0)

    it "makes a vector facing due east face due north" $ do
      rotate90CCwAround0 (10,0) `shouldBe` (0,10)

    it "works for another example" $ do
      rotate90CCwAround0 (-5,13) `shouldBe` (-13,-5)
  describe "applyDirection2" do
    it "matches the example" do
      let Just dirs = parseDirections $ unlines [ 
                           "F10"
                         , "N3"
                         , "F7"
                         , "R90"
                         , "F11"
                         ]
      scanl' applyDirection2 initialState2 dirs `shouldBe`
        [ initialState2
        , ShipState2 (100,10) (10,1)
        , ShipState2 (100,10) (10,4)
        , ShipState2 (170,38) (10,4)
        , ShipState2 (170,38) (4,-10)
        , ShipState2 (214,-72) (4,-10)
        ]
