#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

module Main where
import Data.List     as DL  (reverse, transpose, foldl', nub)

main :: IO ()
main = do
  inputs <- lines <$> getContents
  putStrLn $ unlines (show . naive  4 0 <$> inputs)
  putStrLn $ unlines (show . naive 14 0 <$> inputs)

naive :: Int -- ^ how many characters need to be unique?
      -> Int -- ^ what is the current character location of the start of the sequence?
      -> String -- ^ what is the remaining string to be compared?
      -> Int    -- ^ location of the rightmost character
naive r loc xs
  | Prelude.length xs < r = error $ "reached end of input string, no unique substrings of length " ++ show r ++ " found"
  | distinct  = loc + r
  | otherwise = naive r (loc+1) (drop 1 xs)
  where distinct = Prelude.length (DL.nub (take r xs)) == r
