{-# LANGUAGE TupleSections #-}

module Lib10 where
import qualified Data.List   as DL
import Control.Monad
import Control.Monad.Trans.State

main :: IO ()
main = do
  input <- fmap words <$> (lines <$> getContents)
  let log = scanl tick (1,1) input
  print $ fillGaps log
  print $ strength `at` log <$> [20,60..220]
  print $ foldl1 (+) (strength `at` log <$> [20,60..220])

  forM_ (fillGaps log) $ \(c,x) -> do
    let width = 40
        xpos = ((c-1) `mod` width)
    putStr =<< brightDark (c,x) xpos
    when (xpos == width - 1) $ putStr "\n"

type Cycle = Int
type Register = Int
type RegisterAt = (Cycle,Register)
type Instruction = [String]
type Strength = Int

tick :: RegisterAt -> Instruction -> RegisterAt
tick (c,x) ["addx",ns]  = (c+2,x + read ns)
tick (c,x) _ {- noop -} = (c+1,x)

strength :: RegisterAt -> Cycle -> Strength
strength (cx,rx) c = c * rx

at :: (RegisterAt -> Cycle -> a) -> [RegisterAt] -> Cycle -> a
at f log c =
  let ((c1,x1),(c2,x2)) = head $ dropWhile (\((c1,x1),(c2,x2)) -> c2 < c) (zip ((0,0):log) log)
  in if c == c2
     then f (c2,x2) c
     else f (c1,x1) c

-- the "at" approach has the potential to be quadratic.
-- Let's try a different style, that expands all the gaps in the log, so we can O(n) through it.

fillGaps :: [RegisterAt] -> [RegisterAt]
fillGaps [] = []
fillGaps [cx] = [cx]
fillGaps ((c1,x1):(c2,x2):cxs) = if c2 - c1 == 1
                                 then (c1,x1) :              fillGaps ((c2,x2):cxs)
                                 else (c1,x1) : (c1+1, x1) : fillGaps ((c2,x2):cxs)

brightDark :: RegisterAt -> Int -> IO String
brightDark (cx,rx) c = do
  let out = if abs (c - rx) <= 1
            then "#"
            else " "
  return out
  
