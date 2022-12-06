#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

{-# LANGUAGE BangPatterns #-}

-- https://adventofcode.com/2019/day/16 is said to be one of the harder ones
module Main where
import Prelude       hiding (head, tail)
import Data.Vector   as Vec
import Data.Maybe           (mapMaybe, fromMaybe)
import qualified Data.List as DL
import Control.Monad as M       (forM_, foldM)
import GHC.Utils.Misc       (nTimes)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)

-- first, we switch to Data.Matrix
-- then we use Strassen's algorithm
-- then we try https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch06.html

str2int :: String -> Vector Int
str2int xs = Vec.fromList $ read . (:[]) <$> xs

int2str :: Vector Int -> String
int2str = Prelude.concatMap show

patt :: Vector Int -> [Int] -> Int
patt xs ys = abs (Vec.sum (uncurry (*) <$> Vec.zip xs (Vec.fromList $ DL.tail $ cycle ys))) `mod` 10

baseFor :: Int -> [Int]
baseFor n = DL.concatMap (DL.replicate n) $ [0,1,0,-1]

-- [TODO]; add a State monad with
-- - a Map from phase value to count index
-- - a Vector from count index to phase value
-- If we detect a repeat we know the cycle period, and from that can extrapolate the final answer from the Vector

data ST = ST { phase2index :: Map.Map String Int
             , index2phase :: Vector String
             }
defaultState :: ST
defaultState = ST mempty mempty

type Op = StateT ST IO

go :: Vector Int -> IO (Vector Int)
go xs = do
  startTimeG <- getPOSIXTime
  let !b4 = baseFor <$> Vec.fromList [1 .. Vec.length xs]
  -- endTimeG <- getPOSIXTime
  -- putStrLn $ "go: computed baseFor length " <> show (Vec.length xs) <> " in " <> show (endTimeG-startTimeG)

  let !toreturn = patt xs <$> b4
  -- endTimeG2 <- getPOSIXTime
  -- putStrLn $ "go " <> show(Vec.length xs) <> ": returning after patt, " <> show (endTimeG2 - startTimeG)

  return toreturn

nest :: (Monad m) => Int -> (a -> m a) -> a -> m a
nest n f x0 = M.foldM (\x () -> f x) x0 (DL.replicate n ())

main :: IO ()
main = do
  [inputS] <- lines <$> getContents
  let input = str2int inputS
  startTime1 <- getPOSIXTime
  putStrLn =<< int2str <$> nest 100 go input
  endTime1 <- getPOSIXTime
  putStrLn $ "part 1: input length " <> show (Prelude.length inputS) <> ". elapsed time: " <> show (endTime1 - startTime1)

  M.forM_ ([500, 700 .. 10000]) $ \l -> do
    startTime2 <- getPOSIXTime
    let input2 = Vec.fromList (Prelude.take l (cycle (Vec.toList input)))
    outputList <- int2str <$> nest 100 go input2
    putStrLn $ outputList
    endTime2 <- getPOSIXTime
    putStrLn $ "part 2: input length " <> show (Vec.length input2) <> ". elapsed time: " <> show (endTime2 - startTime2)


    let offset = (read . int2str $ Vec.take 7 input) :: Int
    putStrLn $ "outputList has length " <> show (Prelude.length outputList)
    putStrLn $ "dropping " <> show offset
    putStrLn $ Prelude.take 8 $ Prelude.drop offset outputList
    putStrLn $ "done"

