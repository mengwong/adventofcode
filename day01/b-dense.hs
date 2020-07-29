#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- here we use parentheses to give familiar comfort, resembling traditional function calls
-- we introduce a lambda, and <$> notation for map / fmap

main = do
  input <- getContents
  print(sum( (\givenMass -> givenMass `div` 3 - 2) <$> read <$> lines input))


