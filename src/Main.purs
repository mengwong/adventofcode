module Main where

import Prelude
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (filter)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let split = splitPosAndNeg [1, -2, 3, -4, 5, -6, -7, 8]
      pos = fst split
      neg = snd split
  log ("pos = " <> show pos)
  log ("neg = " <> show neg)

splitPosAndNeg :: Array Int -> Tuple (Array Int) (Array Int)
splitPosAndNeg xs = Tuple (filter (\x -> x>=0) xs)
                          (filter (\x -> x< 0) xs)
