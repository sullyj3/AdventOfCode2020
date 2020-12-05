module Lib
    ( intList
    , (.:)
    , count
    ) where

import Data.List ( sort
                 , nub)

-- warning - partial
intList :: String -> [Int]
intList = map read . lines


(.:) = (.) . (.)

count = length .: filter

allDistinct xs = (length . nub . sort) xs == (length . sort) xs
