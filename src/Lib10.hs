module Lib10 where

main :: IO ()
main = do
  input <- fmap words <$> (lines <$> getContents)
  let log = tick 1 1 input; width = 40
  print $ sum [ c * x | (c,x) <- log, c `elem` [20,60..220] ]
  putStrLn $ concat [ pixel (c,x) xpos ++ if xpos == width - 1 then "\n" else ""
                    | (c,x) <- log, let xpos = ((c-1) `mod` width) ]
  where tick c x (["addx",ns]:xs)  = let x' = x + read ns in (c+1,x) : (c+2,x') : tick (c+2) x' xs
        tick c x (["noop"]   :xs)  =                         (c+1,x) :            tick (c+1) x  xs
        tick c x []                = []
        pixel (cx,rx) c | abs (c - rx) <= 1 = "#"
                        | otherwise         = " "
