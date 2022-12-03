#!/usr/bin/env stack
-- stack --resolver lts-19.33 script

import Data.List
import Data.List.Split
import Data.Char (ord)
import Data.Maybe

priority :: Char -> Int
priority c = if   o >= ord 'a'
             then o  - ord 'a' + 1
             else o  - ord 'A' + 27
  where o  = ord c

main :: IO ()
main = do
  input <- lines <$> getContents
  let part1 = [ (lhs, rhs, repeated, repeated)
               | line <- input
               , let halfLength = length line `div` 2
                     lhs = nub $ take halfLength line
                     rhs = nub $ drop halfLength line
                     repeated = inAll [lhs, rhs]
               ]
  -- putStr $ unlines $ show <$> part1
  putStrLn $ "part 1: " ++ show (sum $ priority <$> catMaybes [ s | (_,_,_,s) <- part1 ])

  putStrLn $ "part 2: " ++ show (sum $ priority <$> catMaybes (inAll <$> chunksOf 3 input))

-- which element is common to all the lists?
inAll :: (Eq a) => [[a]] -> Maybe a
inAll (x:xs) = listToMaybe [ c | c <- x, all (c `elem`) xs ]
