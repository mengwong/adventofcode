#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- let's outsource our recursion to unfoldr! https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#g:9
import Debug.Trace
import Data.List (unfoldr)
main = do
  modules <- getContents >>= return . map read . lines
  print $ sum $ map fuelNeeded modules

fuelNeeded mass = sum (takeWhile (>=0) $ unfoldr (\x -> Just (x, x `div` 3 - 2)) mass)
                  - mass
