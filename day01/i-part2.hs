#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- the above incantation makes this program runnable as a script; chmod a+rx i-part2.hs
-- to run: ./i-part2.hs < input.txt
import Debug.Trace

main = do
  input <- getContents                 -- this is the Haskell incantation to read from STDIN.
  let mylines = lines input            -- split input on \n, into an array of individual lines
      modules = map read mylines       -- in this script, read :: String -> Int
      fuels   = map (moreNeeded 0) modules -- for each module, what fuel is needed? see below.
      total   = sum fuels              -- then we take the sum across the output array
  print total                          -- and print it

fuelNeeded mass = mass `div` 3 - 2     -- given a mass, integer-divide by 3 and subtract 2

moreNeeded depth mass
  | mass <= 0 = trace (indent ++ "base case!") 0
  | otherwise = let fN = max 0 $ fuelNeeded mass
                    mN =         moreNeeded (depth+1) fN
                in
                  trace (indent ++ show mass ++ " needs " ++ show fN)
                  (fN + mN)
  where indent = take depth (repeat ' ')
