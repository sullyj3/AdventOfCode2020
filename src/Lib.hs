module Lib where

import Data.List ( sort
                 , nub)
import Data.Semigroup (Endo(..), appEndo, stimes)

-- warning - partial
intList :: String -> [Int]
intList = map read . lines

addVec (a,b) (c,d) = (a+c,b+d)
scale n (a,b) = (n*a,n*b)

-- compose a unary function with a binary function
(.:) = (.) . (.)

count = length .: filter

allDistinct xs = (length . nub . sort) xs == (length . sort) xs

applyN :: Int -> (a -> a) -> (a -> a)
applyN n = appEndo . stimes n . Endo
