#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- let's outsource our recursion to unfoldr! https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#g:9
import Debug.Trace
import Data.List (unfoldr)
main = do
  input <- getContents                 -- this is the Haskell incantation to read from STDIN.
  let mylines = lines input            -- split input on \n, into an array of individual lines
      modules = map read mylines       -- in this script, read :: String -> Int
      fuels   = map fuelNeeded modules -- for each module, what fuel is needed? see below.
      total   = sum fuels              -- then we take the sum across the output array
  print total                          -- and print it

fuelNeeded mass = sum (takeWhile (>=0) $ unfoldr (\x -> Just (x, trace (show x ++ " needs") (x `div` 3 - 2))) mass)
                  - mass
