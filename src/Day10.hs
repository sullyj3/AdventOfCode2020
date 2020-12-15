module Day10 (
  doDay10
  ) where

import Lib
import Data.Semigroup.Foldable
import qualified Data.List.NonEmpty as NE
import           Data.List (sort)

-----------
--PART 1 --
-----------
-- build a data type that keeps track of the number of size 3 and 5 gaps for a 
-- given list segment. Then we just map each adapter in the list to the data 
-- type and combine them together correctly


-- only works on sorted lists
data JoltGaps = JG { jgLeft :: Int
                   , jgRight :: Int
                   , jgSize3Gaps :: Int
                   , jgSize1Gaps :: Int }
  deriving Show


toJoltGaps :: Int -> JoltGaps
toJoltGaps n = JG n n 0 0

instance Semigroup JoltGaps where
  (JG l1 r1 sz3_1 sz1_1) <> (JG l2 r2 sz3_2 sz1_2) =
    JG l1 r2 (sz3_1 + sz3_2 + extraSz3) (sz1_1 + sz1_2 + extraSz1)
    where diff = l2-r1
          extraSz3 = if diff == 3 then 1 else 0
          extraSz1 = if diff == 1 then 1 else 0


part1 :: [Int] -> Int
part1 ns = sz1 * (sz3 + 1) -- add one size 3 gap of max to built in adapter
  where ns' = NE.fromList . sort $ 0:ns -- include 0 for seat outlet
        JG _ _ sz3 sz1 = foldMap1 toJoltGaps ns'

-----------
--PART 2 --
-----------

-- we work backwards through the sorted list of adapters, at each step keeping 
-- track of the current 3 lowest joltage ones, and their respective path counts
-- to the device. To calculate the path count of the next lowest adapter, we take
-- the sum of the path counts of those adapters reachable by the new one.

newtype PathCount = PC Int
-- list of 3 leftmost adapters, and their respective numbers of paths to the end
type JoltPaths = [(Int, PathCount)]


toJoltPaths :: Int -> JoltPaths
toJoltPaths n = [(n, PC 1)]


joltPathsAddLeft :: Int -> JoltPaths -> JoltPaths
joltPathsAddLeft n jps = take 3 $ (n, PC newPaths) : jps
  where newPaths = sum [ pathCount | (m, PC pathCount) <- jps, m-n <=3 ]


part2 :: [Int] -> Int
part2 ns = pathCount
  where deviceAdapter = maximum ns + 3
        ((_, PC pathCount):_) = foldr joltPathsAddLeft 
                                      (toJoltPaths deviceAdapter)
                                      (0:sort ns)


doDay10 :: IO ()
doDay10 = do
  let fp = "inputs/day10.txt"
  ns <- intList <$> readFile fp
  print $ part1 ns
  -- print $ part2 ns
