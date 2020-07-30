#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- the above incantation makes this program runnable as a script; chmod a+rx a-simple.hs
-- to run: ./a-simple.hs < input.txt

main = do
  input <- getContents                 -- this is the Haskell incantation to read from STDIN.
  let mylines = lines input            -- split input on \n, into an array of individual lines
      modules = map read mylines       -- in this script, read :: String -> Int
      fuels   = map  fuelNeeded modules -- for each module, what fuel is needed? see below.
--    fuels   = fmap fuelNeeded modules -- another way of saying it
--    fuels   = fuelNeeded <$> modules  -- yet another way of saying it, which you will see more of later
      total   = sum fuels              -- then we take the sum across the output array
  print total                          -- and print it

fuelNeeded mass = mass `div` 3 - 2     -- given a mass, integer-divide by 3 and subtract 2


