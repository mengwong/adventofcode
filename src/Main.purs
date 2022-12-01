module Main where

import Prelude
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Array (filter)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let pos /\ neg = splitPosAndNeg [1, -2, 3, -4, 5, -6, -7, 8]
  log ("pos = " <> show pos)
  log ("neg = " <> show neg)

splitPosAndNeg :: Array Int -> Tuple (Array Int) (Array Int)
splitPosAndNeg xs = (filter (\x -> x>=0) xs) 
                    /\
                    (filter (\x -> x< 0) xs)

