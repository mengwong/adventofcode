module Lib10 where
import Control.Monad
import Control.Monad.Trans.State

main :: IO ()
main = do
  input <- fmap words <$> (lines <$> getContents)
  let log = fillGaps $ scanl tick (1,1) input; width = 40
  print $ sum [ c * x | (c,x) <- log, c `elem` [20,60..220] ]
  putStrLn $ concat [ brightDark (c,x) xpos ++ if xpos == width - 1 then "\n" else ""
                    | (c,x) <- log, let xpos = ((c-1) `mod` width) ]
  where tick (c,x) ["addx",ns]  = (c+2,x + read ns)
        tick (c,x) _ {- noop -} = (c+1,x)
        fillGaps ((c1,x1):(c2,x2):cxs) = if c2 - c1 == 1
                                         then (c1,x1) :              fillGaps ((c2,x2):cxs)
                                         else (c1,x1) : (c1+1, x1) : fillGaps ((c2,x2):cxs)
        fillGaps cx = cx
        brightDark (cx,rx) c | abs (c - rx) <= 1 = "#"
                             | otherwise         = " "
