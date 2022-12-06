#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

{-# LANGUAGE BangPatterns #-}

-- https://adventofcode.com/2019/day/16 is said to be one of the harder ones
module Main where
import Prelude       hiding (head, tail)
import qualified Data.Vector   as Vec
import Data.Maybe           (mapMaybe, fromMaybe)
import qualified Data.List as DL
import Control.Monad as M       (forM_, foldM)
import GHC.Utils.Misc       (nTimes)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Matrix
import Data.Int
import Numeric
import Data.Bits
import Data.Char (intToDigit)
import Data.Ord

-- we use Data.Matrix, but we still get OOM errors
-- then we try https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch06.html

str2int :: String -> Vec.Vector Int8
str2int xs = Vec.fromList $ read . (:[]) <$> xs

int2str :: Vec.Vector Int8 -> String
int2str = Prelude.concatMap show

baseFor :: Int -> Int -> [Int8]
baseFor len n = take len $ DL.tail $ cycle $ DL.concatMap (DL.replicate n) $ fromIntegral <$> [0,1,0,-1]

baseFor' :: Int -> Int -> [Int8]
baseFor' len n = take len $          cycle $ DL.concatMap (DL.replicate n) $ fromIntegral <$> [0,1,0,-1]

-- [TODO]; add a State monad with
-- - a Map from phase value to count index
-- - a Vec.Vector from count index to phase value
-- If we detect a repeat we know the cycle period, and from that can extrapolate the final answer from the Vec.Vector

data ST = ST { phase2index :: Map.Map String Int
             , index2phase :: Vec.Vector String
             }
defaultState :: ST
defaultState = ST mempty mempty

type Op = StateT ST IO

-- size notes
-- the input has length 650; we are to repeat it 10,000 times, for a total of 6.5M integers.
-- With Int8 that's 6.5M bytes which is not too bad.
-- but the base pattern is tricky.
-- we can set up a sort of peephole linear grind, exploiting fusion, which will run in constant space,
-- but take forever.
-- we can use Data.Matrix to set up a massive base-pattern matrix of 0,1,0,-1;
-- that will be 6.5M*6.5M = 42 terabytes

-- or we can use a generator function to return the appropriate 0,1,0,-1 value of the matrix
-- based on position

-- who wins the leftmost different bit?
myCmp :: Int -> Int -> IO Int
myCmp y x = do
  let y8 = fromIntegral y :: Int8
      x8 = fromIntegral x :: Int8
      xo = y8 `xor` x8
      lg2 = finiteBitSize xo - 1 - countLeadingZeros xo
      yx = y8 .&. xo
      xx = x8 .&. xo
      ww = case compare yx xx of
             LT -> " X"
             GT -> " Y"
             EQ -> " -"
  putStr $ "myCmp: "
    <> asBit xx <> " = xx"
    <> asBit yx <> " = yx"
    <> asBit xo <> " = xo"
    <> " " <> ww <> " = winner   "
  return 2

asBit :: (Show a, Integral a) => a -> String
asBit n
  | n < 0     = replicate 7 '!'
  | otherwise = leftpad 7 (showIntAtBase 2 intToDigit n "")

genBase :: (Int, Int) -> Matrix Int8 -> IO Int8
genBase (y,x) mat = do
  let e = getElem y x mat
  g <- -- first leftmost position which differs between x and y; basically a cmp
       myCmp y x
  putStrLn ((if e < 0 then "" else " ") <>
            show e <> " <- " <>
            asBit ( y `xor` x) <> "=xor" <> " " <>
            asBit ( y  .&.  x) <> "=and" <> " " <>
            asBit ( y  .|.  x) <> "=or"  <> "; " <>
            "y " <> (show y) <> "=" <> asBit y <> ", " <>
            "x " <> (show x) <> "=" <> asBit x <> ";  "
           )
  return 1

leftpad :: Int -> String -> String
leftpad n s = replicate (n - length s) ' ' ++ s

go :: Matrix Int8 -> Vec.Vector Int8 -> IO (Vec.Vector Int8)
go b4 xs = do
  let vecProd = multStd2 b4 (colVector xs)
  let toreturn = getMatrixAsVector $ fmap (\n -> abs n `mod` 10) vecProd
  return toreturn

nest :: (Monad m) => Int -> (a -> m a) -> a -> m a
nest n f x0 = M.foldM (\x () -> f x) x0 (DL.replicate n ())

main :: IO ()
main = do
  let l = 8
      b4 = fromLists $ baseFor l <$> [1 .. l]
  sequence [ genBase (y,x) b4
           | y <- [1..l]
           , x <- [1..l]
           ]
  print b4
  return ()

{-
main :: IO ()
main = do
  [inputS] <- lines <$> getContents
  let input = str2int inputS
  startTime1 <- getPOSIXTime
  let b4 = fromLists $ baseFor (Vec.length input) <$> [1 .. Vec.length input]
  print b4
  putStrLn =<< int2str <$> nest 100 (go b4) input
  endTime1 <- getPOSIXTime
  putStrLn ("part 1: input length " <> show (Prelude.length inputS) <>
            ". elapsed time: " <> show (endTime1 - startTime1))

  M.forM_ ([500,1000 .. length input * 10000]) $ \l -> do
    startTime2 <- getPOSIXTime
    let input2 = Vec.fromList (Prelude.take l (cycle (Vec.toList input)))
    let b4 = fromLists $ baseFor l <$> [1 .. l]
    outputList <- int2str <$> nest 100 (go b4) input2

    let offset = (read . int2str $ Vec.take 7 input) :: Int
    -- putStrLn $ "outputList has length " <> show (Prelude.length outputList)
    putStrLn $ Prelude.take 8 $ Prelude.drop offset outputList
    endTime2 <- getPOSIXTime
    putStrLn ("part 2: input length " <> show (Vec.length input2) <>
              ". elapsed time: " <> show (endTime2 - startTime2))

-}
  
