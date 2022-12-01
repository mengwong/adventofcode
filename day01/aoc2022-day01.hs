#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

import Data.List
import Data.Tuple
import Data.Function (on)
import Data.List.Split

main :: IO ()
main = do
  paras <- splitOn [""] <$> lines <$> getContents
  let fellowship = zip [1..] ( fmap read <$> paras )
  -- [ (1, [11,22,33])
  -- , (2, [44,55,66]) ]
  putStr $ unlines $
    ( showDwarf <$> fellowship) ++
    let sorted = reverse (sortBy (compare `on` (sum . snd)) fellowship)
        fattest = uncons sorted
    in [ maybe "there are no dwarves!"
         (("the fattest " ++) . showDwarf . fst)
         fattest ] ++
       (showDwarf <$> take 3 sorted) ++
       [ "together, the three fattest hold " ++ show (sum (snd <$> (fmap sum <$> (take 3 sorted)))) ++ " calories" ]

  where
    showDwarf (i,asInts) = "dwarf " ++ show i ++ " holds " ++ show (sum asInts) ++ " calories: " ++ show asInts

