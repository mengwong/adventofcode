#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- we build a list of list of Ints, and need to sum twice
import Debug.Trace
import Data.List (unfoldr)
main = do
  modules <- getContents >>= return . map read . lines
  print $ (sum $ sum <$> fuelNeeded <$> modules ) - sum modules

fuelNeeded :: Int -> [Int]
fuelNeeded = unfoldr (\x -> if x <= 0 then Nothing else Just (x, x `div` 3 - 2))
