#!/usr/bin/env stack
-- stack --resolver lts-19.33 script

import Data.List.Split (splitOn)

contains :: (Ord a) => [a] -> [a] -> Bool
a `contains` b = minimum a <= minimum b && maximum b <= maximum a
--                       a------------b============b------------a
overlaps :: (Ord a) => [a] -> [a] -> Bool
a `overlaps` b = minimum a <= minimum b && minimum b <= maximum a
--                       a------------b=========================a=====b
--                       a------------b-------------------------a
-- for full theoretical treatment, see Allen's Interval Algebra
main :: IO ()
main = do
  input <- lines <$> getContents
  let elves = [ (elf1,elf2) | line <- input, let [elf1, elf2] = fmap (read :: String -> Int) . splitOn "-" <$> splitOn "," line ]
  print $ length $ filter id [ orViceVersa elf1 contains elf2 | (elf1,elf2) <- elves ]
  print $ length $ filter id [ orViceVersa elf1 overlaps elf2 | (elf1,elf2) <- elves ]
  where orViceVersa x f y = f x y || f y x
