#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- while (+2) would work, it is necessary to say (subtract 2), because (-2) is just a number

main = do
  input <- getContents
  print $ sum $ ((subtract 2) . (`div` 3)) <$> read <$> lines input
