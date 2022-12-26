#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

module Main where

import qualified Data.Vector   as DV
-- import Text.Megaparsec      (parseMaybe, many, some, Parsec)
-- import Text.Megaparsec.Char (char, numberChar, upperChar, space, string)
-- import Data.Maybe           (mapMaybe, fromMaybe)
import Data.List     as DL  (reverse, transpose, foldl', nub)
-- import Data.List.Split      (splitOn)
import Control.Monad        (forM_)

-- type Parser = Parsec () String

main :: IO ()
main = do
  inputs <- lines <$> getContents
  putStrLn $ unlines (show . naive  4 0 <$> inputs)
  putStrLn $ unlines (show . naive 14 0 <$> inputs)


-- | naive algorithm.
--
-- at each character, review the last four characters; are they all distinct? if so, report character index.
naive :: Int -- ^ how many characters need to be unique?
      -> Int -- ^ what is the current character location of the start of the sequence?
      -> String -- ^ what is the remaining string to be compared?
      -> Int    -- ^ location of the rightmost character
naive r loc xs
  | Prelude.length xs < r = error $ "reached end of input string, no unique substrings of length " ++ show r ++ " found"
  | distinct  = loc + r
  | otherwise = naive r (loc+1) (drop 1 xs)
  where distinct = Prelude.length (DL.nub (take r xs)) == r

-- | sophisticated algorithm.
--
-- maintain a running lookup table of the last time a certain character was found.
-- if all of the most recent N(=4) characters have recency distance > N then return the current character index.

int :: String -> Int
int = read
