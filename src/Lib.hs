module Lib
    ( intList
    , (.:)
    , count
    ) where

-- warning - partial
intList :: String -> [Int]
intList = map read . lines


(.:) = (.) . (.)

count = length .: filter

allDistinct xs = (length . nub . sort) xs == (length . sort) xs
