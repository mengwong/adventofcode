#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

{-# LANGUAGE Strict, BangPatterns #-}

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
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Int
import System.Mem

-- then we try https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch06.html

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

go :: Int -> Vector Int8 -> IO (Vector Int8)
go g xs = do
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
  when (modn g) $
    putStrLn $ "go: " <> show (endTime-startTime) <> ": run " <> show g -- <> " returning " <> int2str toreturn

  return $! toreturn

-- | similar to nTimes, but for monads; run a given monad a certain number of times against its own output
nest :: (Monad m) => Int -> (Int -> a -> m a) -> a -> m a
nest n f x0 = M.foldM (\x n' -> f n' x) x0 [1..n]

main :: IO ()
main = do
  [inputS] <- lines <$> getContents
  let input = str2int inputS

  putStrLn $ "* part 1"
  startTime1 <- getPOSIXTime
  putStrLn =<< Prelude.take 8 . int2str <$> nest 100 go input
  endTime1 <- getPOSIXTime
  putStrLn $ "** input length " <> show (Prelude.length inputS) <> ". elapsed time: " <> show (endTime1 - startTime1)

  putStrLn $ "* part 2"

  M.forM_ ((Vec.length input *) <$> [1,10,100,1000,10000]) $ \l -> do
    putStrLn $ "** constructing input array of length " <> show l
    startTime2 <- getPOSIXTime
    let input2 = Vec.fromList $ Prelude.take l (cycle (Vec.toList input))

    midTime2 <- getPOSIXTime
    putStrLn $ "** constructed input array of length " <> show l <> ", took " <> show (midTime2-startTime2)
    outputList <- nest 100 go input2

    midTime2b <- getPOSIXTime
    putStrLn $ "** ran 100 times, took " <> show (midTime2b-startTime2) <> "; now dropping from the outputlist"

    let offset = (read . int2str $ Vec.take 7 input) :: Int
        result = int2str $ Vec.take 8 $ Vec.drop offset outputList
    unless (Prelude.null result) $
      putStrLn result

    endTime2 <- getPOSIXTime
    putStrLn $ "** part 2: done with input length " <> show (Vec.length input2) <> ". elapsed time: " <> show (endTime2 - startTime2)

