module Lib10 where
import Data.List.Split
main :: IO ()
main = do
  input <- (fmap words . lines) <$> getContents
  let log = tick 1 1 input; width = 40
  print $ sum [ cx * rx | (cx,rx) <- log, cx `elem` [20,60..220] ]
  putStrLn $ unlines $ chunksOf width [ pixel (cx,rx) xpos
                                      | (cx,rx) <- log, let xpos = ((cx-1) `mod` width) ]
  where tick cx rx (["addx",ns]:xs)  = let rx' = rx + read ns in (cx,rx) : (cx+1,rx) : tick (cx+2) rx' xs
        tick cx rx (["noop"]   :xs)  =                           (cx,rx) :             tick (cx+1) rx  xs
        tick cx rx []                = []
        pixel (cx,rx) c | abs (c - rx) <= 1 = '#'
                        | otherwise         = ' '
