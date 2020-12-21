module Lib where

import Data.List ( sort
                 , nub)
import Data.Foldable (minimumBy)
import Data.Semigroup (Endo(..), appEndo, stimes)
import Data.Ord (comparing)
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

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

minimumOn f = minimumBy (comparing f)

toFst f x = (f x, x)
toSnd f x = (x, f x)


replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map replaceIf
  where replaceIf x | x == a    = b
                    | otherwise = x


readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
