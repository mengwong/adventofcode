#!/usr/bin/env stack
-- stack --resolver lts-16.2 script

main = do
  input <- getContents
  print $ sum $ ((subtract 2) . (`div` 3)) <$> read <$> lines input
