#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- "point-free" notation is confusingly named, because there are lots of dots here

main = getContents >>= print . sum . fmap ((subtract 2).(`div` 3)) . fmap read . lines
