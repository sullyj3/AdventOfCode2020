{-# LANGUAGE BlockArguments #-}

module TestDay11 where


import Test.Hspec
--import Control.Exception (evaluate)

import Data.List (intersperse)
import Day11

main :: IO ()
main = hspec $ do
  describe "updateCellPart2" $ do
    let initial = toGrid [
            "L.LL.LL.LL"
          , "LLLLLLL.LL"
          , "L.L.L..L.."
          , "LLLL.LL.LL"
          , "L.LL.LL.LL"
          , "L.LLLLL.LL"
          , "..L.L....."
          , "LLLLLLLLLL"
          , "L.LLLLLL.L"
          , "L.LLLLL.LL"
          ]
        stepper = runStep updateCellPart2
    it "matches the sequence described in the problem description" $ do
      let expectedSequence = toGrid <$> [
              [ "L.LL.LL.LL"
              , "LLLLLLL.LL"
              , "L.L.L..L.."
              , "LLLL.LL.LL"
              , "L.LL.LL.LL"
              , "L.LLLLL.LL"
              , "..L.L....."
              , "LLLLLLLLLL"
              , "L.LLLLLL.L"
              , "L.LLLLL.LL"
              ]
            , [ "#.##.##.##"
              , "#######.##"
              , "#.#.#..#.."
              , "####.##.##"
              , "#.##.##.##"
              , "#.#####.##"
              , "..#.#....."
              , "##########"
              , "#.######.#"
              , "#.#####.##"
              ]
            , [ "#.LL.LL.L#"
              , "#LLLLLL.LL"
              , "L.L.L..L.."
              , "LLLL.LL.LL"
              , "L.LL.LL.LL"
              , "L.LLLLL.LL"
              , "..L.L....."
              , "LLLLLLLLL#"
              , "#.LLLLLL.L"
              , "#.LLLLL.L#"
              ]
            , [ "#.L#.##.L#"
              , "#L#####.LL"
              , "L.#.#..#.."
              , "##L#.##.##"
              , "#.##.#L.##"
              , "#.#####.#L"
              , "..#.#....."
              , "LLL####LL#"
              , "#.L#####.L"
              , "#.L####.L#"
              ]
            , [ "#.L#.L#.L#"
              , "#LLLLLL.LL"
              , "L.L.L..#.."
              , "##LL.LL.L#"
              , "L.LL.LL.L#"
              , "#.LLLLL.LL"
              , "..L.L....."
              , "LLLLLLLLL#"
              , "#.LLLLL#.L"
              , "#.L#LL#.L#"
              ]
            , [ "#.L#.L#.L#"
              , "#LLLLLL.LL"
              , "L.L.L..#.."
              , "##L#.#L.L#"
              , "L.L#.#L.L#"
              , "#.L####.LL"
              , "..#.#....."
              , "LLL###LLL#"
              , "#.LLLLL#.L"
              , "#.L#LL#.L#"
              ]
            , [ "#.L#.L#.L#"
              , "#LLLLLL.LL"
              , "L.L.L..#.."
              , "##L#.#L.L#"
              , "L.L#.LL.L#"
              , "#.LLLL#.LL"
              , "..#.L....."
              , "LLL###LLL#"
              , "#.LLLLL#.L"
              , "#.L#LL#.L#"
              ]
            ]
      let actualSequence = take 7 (iterate stepper initial)
      putStrLn "expected vs actual:"
      let showAll :: [Grid] -> String
          showAll = concat . intersperse "\n" . map showGrid
      putStrLn $ hConcat (showAll expectedSequence) (showAll actualSequence)
      expectedSequence `shouldBe` actualSequence

    it "small example 1" $ do
      let initial = toGrid
              [ "#.##.##.##"
              , "#######.##"
              , "#.#.#..#.."
              , "####.##.##"
              , "#.##.##.##"
              , "#.#####.##"
              , "..#.#....."
              , "##########"
              , "#.######.#"
              , "#.#####.##"
              ]
      updateCellPart2 (0,0) initial `shouldBe` '#'

  describe "visibleChairs" $ do
    it "small example 1" $ do
      let g = toGrid
              [ "#.##.##.##"
              , "#######.##"
              , "#.#.#..#.."
              , "####.##.##"
              , "#.##.##.##"
              , "#.#####.##"
              , "..#.#....."
              , "##########"
              , "#.######.#"
              , "#.#####.##"
              ]
      visibleChairs (0,0) g `shouldBe` "###"


hConcat :: String -> String -> String
hConcat s1 s2 = unlines $ zipWith (\l1 l2 -> l1++"   "++l2) (lines s1) (lines s2)
