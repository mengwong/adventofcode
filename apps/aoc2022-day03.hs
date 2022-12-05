#!/usr/bin/env stack
-- stack --resolver lts-19.33 script

module Main where
import Data.List.Split (chunksOf)
import Data.Char (ord)
import Data.Maybe (listToMaybe, mapMaybe)

priority :: Char -> Int
priority c = if   ord c >= ord 'a'
             then ord c  - ord 'a' + 1
             else ord c  - ord 'A' + 27

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ go [ chunksOf (length line `div` 2) line | line <- input ]
  print $ go ( chunksOf                    3                 input )
  where   go xs = sum $ priority <$> mapMaybe common xs

-- which element is common to all the input lists?
common :: (Eq a) => [[a]] -> Maybe a
common (x:xs) = listToMaybe [ c | c <- x, (c `elem`) `all` xs ]
common _ = Nothing
