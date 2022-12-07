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

-- the next thing to try is https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch06.html

str2int :: String -> Vector Int8
str2int xs = Vec.fromList $ read . (:[]) <$> xs

int2str :: Vector Int8 -> String
int2str = Prelude.concatMap show . Vec.toList

genBase :: Int -> Int -> Int8
genBase y x = do
  case x `div` y `mod` 4 of
    0 ->   0
    1 ->   1
    2 ->   0
    3 -> (-1)

go :: POSIXTime -> Int -> Vector Int8 -> IO (Vector Int8)
go start0 g xs = do
  performGC
  let l = Vec.length xs
      modn n = n `mod` 10 == 0
  startTime <- getPOSIXTime
  let !toreturn = Vec.fromList
                  [ abs (Vec.sum (Vec.imap (\x n -> genBase y (x+1) * n) xs))
                    `mod` 10
                  | y <- [ 1 .. l ]
                  ]
  endTime <- getPOSIXTime
  when (modn g) $ do
    putStrLn $ "go: " <> show (endTime-startTime) <> ": run " <> show g <> " (" <> show (endTime-start0) <> " since start)"
    -- <> " returning " <> int2str toreturn

  return $! toreturn

-- | similar to nTimes, but for monads; run a given monad a certain number of times against its own output.
-- we previously tried `nest` but something about the fold ate too much ram.
nest :: (Monad m) => Int -> (Int -> a -> m a) -> a -> m a
nest n f x0 = M.foldM (\x n' -> f n' x) x0 [1..n]


{- |roll-your-own with as much strictness as possible.

but somewhere in nTimesM (go...) is a memory leak causing slowdown

** constructed input array of length 320000, took 0s
go: 0.02715s: run 100 (0.032702s since start)
go: 0.013188s: run 90 (0.506888s since start)
go: 0.012969s: run 80 (1.560669s since start)
go: 0.012629s: run 70 (3.191519s since start)
go: 0.012641s: run 60 (5.409715s since start)
go: 0.013199s: run 50 (8.225859s since start)
go: 0.012661s: run 40 (11.638799s since start)
go: 0.012313s: run 30 (15.605208s since start)
go: 0.014037s: run 20 (20.318473s since start)
go: 0.012758s: run 10 (25.562153s since start)
** ran 100 times, took 30.668926s; now dropping from the outputlist
** part 2: done with input length 320000. elapsed time: 30.668969s

-}

nTimesM :: (Monad m) => Int -> (Int -> a -> m a) -> a -> m a
nTimesM n f x
  | n == 0 = return $! x
  | otherwise = nTimesM (n-1) f =<< f n x


main :: IO ()
main = do
  [inputS] <- lines <$> getContents
  let input = str2int inputS

  putStrLn $ "* part 1"
  startTime1 <- getPOSIXTime
  putStrLn =<< Prelude.take 8 . int2str <$> nTimesM 100 (go startTime1) input
  endTime1 <- getPOSIXTime
  putStrLn $ "** input length " <> show (Prelude.length inputS) <> ". elapsed time: " <> show (endTime1 - startTime1)

  putStrLn $ "* part 2"

  M.forM_ ((Vec.length input *) <$> [1,10,100,1000,10000]) $ \l -> do
    startTime2 <- getPOSIXTime
    let input2 = Vec.fromList $ Prelude.take l (cycle (Vec.toList input))

    midTime2 <- getPOSIXTime
    putStrLn $ "** constructed input array of length " <> show l <> ", took " <> show (midTime2-startTime2)
    outputList <- nTimesM 100 (go midTime2) input2

    midTime2b <- getPOSIXTime
    putStrLn $ "ran 100 times, took " <> show (midTime2b-startTime2) <> "; now dropping from the outputlist"

    let offset = (read . int2str $ Vec.take 7 input) :: Int
        result = int2str $ Vec.take 8 $ Vec.drop offset outputList
    unless (Prelude.null result) $
      putStrLn "* answer: result"

    endTime2 <- getPOSIXTime
    putStrLn $ "*** part 2: done with input length " <> show (Vec.length input2) <> ". elapsed time: " <> show (endTime2 - startTime2)

