module Day10 (
  doDay10
  ) where

import Lib
import Data.Semigroup.Foldable
import qualified Data.List.NonEmpty as NE
import           Data.List (sort)

-- only works on sorted lists
data JoltGaps = JG { jgLeft :: Int
                   , jgRight :: Int
                   , jgSize3Gaps :: Int
                   , jgSize1Gaps :: Int
                   , jgMax :: Int }
  deriving Show

-- list of 3 leftmost adaptors, and their respective numbers of paths to the end
newtype PathCount = PC Int
type JoltPaths = [(Int, PathCount)]


toJoltPaths :: Int -> JoltPaths
toJoltPaths n = [(n, PC 1)]

{-

-}

{-
[(49, 1)]
49

48
[(48,1), (49,1)]
48 49

47
[(47,2), (48,1), (49,1)]
47 48 49
47 49

46
[(46,3), (47,2), (48,1)]
46 49
46 48 49
46 47 48 49
46 47 49

45
[(45,7), (46,4), (47,2)]

-}

joltPathsAddLeft :: Int -> JoltPaths -> JoltPaths
joltPathsAddLeft n jps = take 3 $ (n, PC newPaths) : jps
  where newPaths = sum [ pathCount | (m, PC pathCount) <- jps, m-n <=3 ]


toJoltGaps :: Int -> JoltGaps
toJoltGaps n = JG n n 0 0 n

instance Semigroup JoltGaps where
  (JG l1 r1 sz3_1 sz1_1 max1) <> (JG l2 r2 sz3_2 sz1_2 max2) =
    JG l1 r2 (sz3_1 + sz3_2 + extraSz3) (sz1_1 + sz1_2 + extraSz1) (max max1 max2)
    where diff = l2-r1
          extraSz3 = if diff == 3 then 1 else 0
          extraSz1 = if diff == 1 then 1 else 0


part1 :: [Int] -> Int
part1 ns = sz1 * (sz3 + 1) -- add one size 3 gap of max to built in adapter
  where ns' = NE.fromList . sort $ 0:ns -- include 0 for seat outlet
        JG _ _ sz3 sz1 _ = foldMap1 toJoltGaps ns'

part2 :: [Int] -> Int
part2 ns = pathCount
  where deviceAdapter = maximum ns + 3
        ((_, PC pathCount):_) = foldr joltPathsAddLeft 
                                      (toJoltPaths deviceAdapter)
                                      (0:sort ns)

-- strategy for part2: work backwards through the sorted list.
-- find number of paths from max adapter to device adapter (1)
-- find number of paths from 2nd highest adapter to max adapter
-- paths from 3rd highest is paths from 2nd highest to max + 1 if we can skip.


doDay10 :: IO ()
doDay10 = do
  let fp = "inputs/day10.txt"
  ns <- intList <$> readFile fp
  -- print $ part1 ns
  print $ part2 ns
