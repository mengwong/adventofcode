{-# LANGUAGE TupleSections #-}

module Lib10 where
import qualified Data.List   as DL
import Control.Monad
import Control.Monad.Trans.State
import Debug.Trace

main :: IO ()
main = do
  input <- fmap words <$> (lines <$> getContents)
  let log = scanl tick (1,1) input
  print log
  print $ strengthAt log <$> [20,60..220]
  print $ foldl1 (+) (strengthAt log <$> [20,60..220])
type Cycle = Int
type Register = Int
type RegisterAt = (Cycle,Register)
type Instruction = [String]
type Strength = Int

tick :: RegisterAt -> Instruction -> RegisterAt
tick (c,x) ["noop"]    = (c+1,x)
tick (c,x) ["addx",ns] = (c+2,x + read ns)

strengthAt :: [RegisterAt] -> Cycle -> Strength
strengthAt log c =
  let ((c1,x1),(c2,x2)) = head $ dropWhile (\((c1,x1),(c2,x2)) -> c2 < c) (zip ((0,0):log) log)
  in trace ("c1 x1 = " ++ show (c1,x1) ++ "; c2 x2 = " ++ show (c2,x2)) $
     if c == c2 then c * x2 else c * x1
