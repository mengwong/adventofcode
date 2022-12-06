#!/usr/bin/env stack
-- stack --resolver lts-20.2 script
-- https://adventofcode.com/2019/day/16 is said to be one of the harder ones
module Main where
import Prelude       hiding (head, tail, drop, take)
import Data.Vector   as Vec
import Data.Maybe           (mapMaybe, fromMaybe)
import qualified Data.List as DL
import Control.Monad        (forM_)
import GHC.Utils.Misc       (nTimes)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)


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

go :: Vector Int -> Vector Int
go xs = patt xs <$> baseFor <$> Vec.fromList [1 .. Vec.length xs]

main :: IO ()
main = do
  [inputS] <- lines <$> getContents
  let input = str2int inputS
  startTime <- getPOSIXTime
  putStrLn $ int2str $ nTimes 1 go input
  endTime <- getPOSIXTime
  putStrLn $ "input length " <> show (Prelude.length inputS) <> ". elapsed time: " <> show (endTime - startTime)

{-
  outputList <- liftIO $ sequence $ nTimes 100 go $ concat $ replicate 10000 input
  let offset = (read . int2str $ take 7 input) :: Int
  putStrLn $ "outputList has length " ++ show (length outputList)
  putStrLn $ "dropping " ++ show offset
  putStrLn $ int2str $ take 8 $ drop offset outputList
  putStrLn $ "done"
  
-}
