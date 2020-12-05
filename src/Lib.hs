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
