#!/usr/bin/env stack
-- stack --resolver lts-19.33 script

import Data.List.Split (chunksOf)
import Data.Char (ord)
import Data.Maybe (catMaybes, listToMaybe)

priority :: Char -> Int
priority c = if   o >= ord 'a'
             then o  - ord 'a' + 1
             else o  - ord 'A' + 27
  where o  = ord c

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ go [ chunksOf (length line `div` 2) line | line <- input ]
  print $ go ( chunksOf                    3                 input )
  where
    go xs = sum $ priority <$> catMaybes (common <$> xs)

-- which element is common to all the input lists?
common :: (Eq a) => [[a]] -> Maybe a
common (x:xs) = listToMaybe [ c | c <- x, (c `elem`) `all` xs ]
common _ = Nothing
