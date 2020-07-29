#!/usr/bin/env stack
-- stack --resolver lts-16.2 script

main = getContents >>= print . sum . fmap ((subtract 2).(`div` 3)) . fmap read . lines
