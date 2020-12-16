module Lib where

import Data.List ( sort
                 , nub)

-- warning - partial
intList :: String -> [Int]
intList = map read . lines

addVec (a,b) (c,d) = (a+c,b+d)

(.:) = (.) . (.)

count = length .: filter

allDistinct xs = (length . nub . sort) xs == (length . sort) xs
