#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

{-# LANGUAGE BangPatterns #-}

-- https://adventofcode.com/2019/day/16 is said to be one of the harder ones
module Main where
import Prelude       hiding (head, tail)
import Data.Vector   as Vec
import qualified Data.Vector (Vector)
import Data.Maybe           (mapMaybe, fromMaybe)
import qualified Data.List as DL
import Control.Monad as M       (forM_, foldM, unless, when)
import GHC.Utils.Misc       (nTimes)
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Int
import System.Mem
import Data.Fixed
import System.IO (hPutStrLn, stderr)

-- the next thing to try is https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch06.html

str2int :: String -> Vector Int
str2int xs = Vec.fromList $ read . (:[]) <$> xs

int2str :: Vector Int -> String
int2str = Prelude.concatMap show . Vec.toList

-- | generate the base pattern for a given y,x location in the base-pattern matrix 0,1,0,-1
genBase :: Int -> Int -> Int
genBase y x = do
  case x `div` y `mod` 4 of
    0 ->   0
    1 ->   1
    2 ->   0
    3 -> (-1)

-- | the matrix multiplication product of the input vector * the base pattern
genInner :: Vector Int -> Int -> Int -> Int
genInner v y x = genBase y (x+1) * (v Vec.! x)

-- | each cell in the output column is the ones digit of the sum of the matrix multiplication of the input row
genOuter :: Vector Int -> Int -> Int -> Int
genOuter v l y = abs (Vec.sum (Vec.generate l (genInner v (y+1)))) `mod` 10

go :: POSIXTime -> Int -> Vector Int -> IO (Vector Int)
go start0 g xs = do
  performGC
  let l = Vec.length xs
      modn n = n `mod` 10 == 0
  startTime <- getPOSIXTime
  let !toreturn = Vec.generate l (genOuter xs l)
  endTime <- getPOSIXTime
  when (modn g) $ do
    hPutStrLn stderr $ "go: " <> show (endTime-startTime) <> ": run " <> show g <> " (" <> show (endTime-start0) <> " since start)"
    -- <> " returning " <> int2str toreturn

  return $! toreturn

-- | similar to nTimes, but for monads; run a given monad a certain number of times against its own output.
-- we previously tried `nest` but something about the fold ate too much ram.
nest :: (Monad m) => Int -> (Int -> a -> m a) -> a -> m a
nest n f x0 = M.foldM (\x n' -> f n' x) x0 [1..n]


{- |roll-your-own with as much strictness as possible.

but somewhere in nTimesM (go...) is a memory leak causing slowdown

** constructed input array of length 650000, took 0s
go: 0.054046s: run 100 (0.064163s since start)
go: 0.021463s: run 90 (0.924238s since start)
go: 0.021465s: run 80 (2.826029s since start)
go: 0.02117s: run 70 (5.761707s since start)
go: 0.021514s: run 60 (9.760877s since start)
go: 0.021057s: run 50 (14.902953s since start)
go: 0.021391s: run 40 (21.036775s since start)
go: 0.021891s: run 30 (28.292182s since start)
go: 0.021434s: run 20 (36.462654s since start)
go: 0.022907s: run 10 (45.762933s since start)
ran 100 times, took 55.550888s; now dropping from the outputlist

-}

nTimesM :: (Monad m) => Int -> (Int -> a -> m a) -> a -> m a
nTimesM n f x
  | n == 0 = return $! x
  | otherwise = nTimesM (n-1) f =<< f n x


main :: IO ()
main = do
  [inputS] <- lines <$> getContents
  let input = str2int inputS

  hPutStrLn stderr $ "* part 1"
  startTime1 <- getPOSIXTime
  putStrLn =<< int2str <$> nTimesM 100 (go startTime1) input
  endTime1 <- getPOSIXTime
  hPutStrLn stderr $ "** input length " <> show (Prelude.length inputS) <> ". elapsed time: " <> show (endTime1 - startTime1)

  when True $ part2 input

  where
    part2 :: Vector Int -> IO ()
    part2 input = do  
      let offset = (read . int2str $ Vec.take 7 input) :: Int

      hPutStrLn stderr $ "* part 2"

      M.forM_ ((Vec.length input *) <$> [1,10,100,1000
                                        -- ,10000
                                        ]) $ \l -> do
        startTime2 <- getPOSIXTime
        let input2 = Vec.fromList $ Prelude.take l (cycle (Vec.toList input))
        hPutStrLn stderr $ "** input length " <> show l
        outputList <- nTimesM 100 (go startTime2) input2

        midTime2b <- getPOSIXTime
        hPutStrLn stderr $ "ran 100 times, took " <> show (midTime2b-startTime2)
        let answer = Vec.take 8 $ Vec.drop offset outputList
        when (not $ Vec.null answer) $ hPutStrLn stderr $ "* answer: " <> int2str answer

        endTime2 <- getPOSIXTime
        putStrLn $ "*** part 2: done with input length " <> show (Vec.length input2) <> ". elapsed time: " <> show (endTime2 - startTime2) <> "\n"


