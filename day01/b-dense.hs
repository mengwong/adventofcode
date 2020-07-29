#!/usr/bin/env stack
-- stack --resolver lts-16.2 script

main = do
  input <- getContents
  print(sum( (\givenMass -> givenMass `div` 3 - 2) <$> read <$> lines input))


