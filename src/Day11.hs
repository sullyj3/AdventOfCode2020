{-# LANGUAGE BlockArguments #-}

module Day11 where

import Data.Maybe (catMaybes)
import Control.Monad
import Data.Foldable
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Set as S
import           Data.Set (Set)
import Control.Arrow ((&&&))
import Lib
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

type Grid = Vector (Vector Char)

height g = Vec.length g
width  g = Vec.length (g Vec.! 0)

(!) :: Grid -> (Int, Int) -> Char
g ! (i,j) = (g Vec.! i) Vec.! j

addVec (a,b) (c,d) = (a+c,b+d)

rayCast :: (Int, Int) -> (Int, Int) -> Grid -> Maybe Char
rayCast dirVec start g = find (/='.')
                       $ map (g!)
                       $ takeWhile (flip inRange g)
                       -- tail to exclude the starting point
                       $ tail $ iterate (addVec dirVec) start

visibleChairs :: (Int, Int) -> Grid -> [Char]
visibleChairs pt g = catMaybes [rayCast dir pt g | dir <- dirs ]
  where dirs = [ (1,0), (-1,0), (0,1), (0,-1),
                 (1,1), (1,-1), (-1,1), (-1,-1)]


dims :: Grid -> (Int, Int)
dims = height &&& width

runStep :: ((Int, Int) -> Grid -> Char) -> Grid -> Grid
runStep updateCell g = toGrid [ [ updateCell (i,j) g | j <- [0..width g-1]]
                                                     | i <- [0..height g-1] ]


updateCellPart1 :: (Int, Int) -> Grid -> Char
updateCellPart1 pt g = case g ! pt of
  'L' | '#' `notElem` adjs     -> '#'
      | otherwise              -> 'L'
  '#' | count (=='#') adjs >=4 -> 'L'
      | otherwise              -> '#'
  '.' -> '.'
  where adjs = adjacent g pt

updateCellPart2 :: (Int, Int) -> Grid -> Char
updateCellPart2 pt g = case g ! pt of
  'L' | '#' `notElem` visibles     -> '#'
      | otherwise                  -> 'L'
  '#' | count (=='#') visibles >=5 -> 'L'
      | otherwise                  -> '#'
  '.' -> '.'
  where visibles = visibleChairs pt g

runUntilNoChange :: (Grid -> Grid) -> Grid -> Grid
runUntilNoChange stepper g | g' == g = g'
                           | otherwise = runUntilNoChange stepper g'
                           where g' = stepper g


adjacent :: Grid -> (Int, Int) -> [Char]
adjacent g (i, j) = do
  di <- [-1..1]
  dj <- [-1..1]
  guard $ (di,dj) /= (0,0)
  let pt = (i+di, j+dj)
  guard $ inRange pt g
  pure $ g ! pt


inRange :: (Int, Int) -> Grid -> Bool
inRange (i,j) g = i >=0 && i < h && j >=0 && j < w
  where (h,w) = dims g


toGrid :: [[Char]] -> Grid
toGrid = Vec.fromList . map Vec.fromList

showGrid :: Grid -> String
showGrid = unlines . map Vec.toList . Vec.toList

part1 :: Grid -> Int
part1 g = count (=='#') . showGrid . runUntilNoChange (runStep updateCellPart1) $ g

part2 :: Grid -> Int
part2 g = count (=='#') . showGrid . runUntilNoChange (runStep updateCellPart2) $ g

doDay11 :: IO ()
doDay11 = do
  let fp = "inputs/day11.txt"
  -- let fp = "inputs/day11.txt"
  g <- toGrid . lines <$> readFile fp
  print $ part2 g

