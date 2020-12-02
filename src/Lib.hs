module Lib
    ( intList
    ) where

-- warning - partial
intList :: String -> [Int]
intList = map read . lines

