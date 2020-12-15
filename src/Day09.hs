{-# LANGUAGE BlockArguments #-}

module Day09 (
  doDay9
  ) where

import           Data.List (find)
import           Data.Maybe (fromJust)
import qualified Data.Vector as Vec
import           Data.Vector (Vector, (!))

import           Lib
import           Debug.Trace

any2AddTo :: Int -> Vector Int -> Bool
any2AddTo n vec
  | Vec.null vec = False
  | otherwise    =
    let (h, t) = (Vec.unsafeHead vec, Vec.unsafeTail vec)
        target = n-h
     in target `Vec.elem` t || any2AddTo n t

findWeakness :: Int -> Vector Int -> Int
findWeakness preambleLen ns = (ns !) $ fromJust $ flip find [preambleLen..Vec.length ns-1] \i ->
  let precedingN = Vec.slice (i-preambleLen) preambleLen ns
   in not $ any2AddTo (ns ! i) precedingN

doDay9 :: IO ()
doDay9 = do
  -- let fp = "inputs/day9test.txt"
  let fp = "inputs/day9.txt"
  ns <- Vec.fromList . intList <$> readFile fp
  print $ findWeakness 25 ns
