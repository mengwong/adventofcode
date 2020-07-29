#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- more concisely, using if/then/else expression, and moving the clip to fuelNeeded

main = do
  input <- getContents                 -- this is the Haskell incantation to read from STDIN.
  let mylines = lines input            -- split input on \n, into an array of individual lines
      modules = map read mylines       -- in this script, read :: String -> Int
      fuels   = map moreNeeded modules -- for each module, what fuel is needed? see below.
      total   = sum fuels              -- then we take the sum across the output array
  print total                          -- and print it

fuelNeeded mass = max 0 $ mass `div` 3 - 2     -- given a mass, integer-divide by 3 and subtract 2

moreNeeded mass = if mass <= 0 then 0 else fuelNeeded mass + moreNeeded (fuelNeeded mass)
