#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

module Main where
import Data.List ( sortBy )
import Data.Tuple ()
import Data.Function (on)
import Data.List.Split ( splitOn )

main :: IO ()
main = do
  paras <- splitOn [""] <$> lines <$> getContents
  let fellowship = zip [1..] ( fmap read <$> paras )
  -- [ (1, [11,22,33])
  -- , (2, [44,55,66]) ]
  putStr $ unlines $
    ( showDwarf <$> fellowship) ++ [""] ++
    let sorted = reverse (sortBy (compare `on` (sum . snd)) fellowship)
        fattest = head sorted
        n = 3
    in [ "the fattest " ++ showDwarf fattest ] ++
       (showDwarf <$> take n sorted) ++
       [ "together, the " ++ show n ++ " fattest hold " ++
         show (sum $ sum . snd <$> take n sorted) ++
         " calories" ]

  where
    showDwarf (i,asInts) = "dwarf " ++ show i ++ " holds " ++ show (sum asInts) ++ " calories: " ++ show asInts

