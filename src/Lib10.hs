module Lib10 where
import Data.List.Split
main :: IO ()
main = do
  input <- (fmap words . lines) <$> getContents
  let log = tick 1 1 input; width = 40
  print $ sum [ c * x | (c,x) <- log, c `elem` [20,60..220] ]
  putStrLn $ unlines $ chunksOf width [ pixel (c,x) xpos
                                      | (c,x) <- log, let xpos = ((c-1) `mod` width) ]
  where tick c x (["addx",ns]:xs)  = let x' = x + read ns in (c,x) : (c+1,x) : tick (c+2) x' xs
        tick c x (["noop"]   :xs)  =                         (c,x) :           tick (c+1) x  xs
        tick c x []                = []
        pixel (cx,rx) c | abs (c - rx) <= 1 = '#'
                        | otherwise         = ' '
