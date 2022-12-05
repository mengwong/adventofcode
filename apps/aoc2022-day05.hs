#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

import Prelude       hiding (head, tail, drop, take)
import Data.Vector   hiding (mapMaybe, forM_, reverse, transpose)
import Text.Megaparsec      (parseMaybe, many, some, Parsec)
import Text.Megaparsec.Char (char, numberChar, upperChar, space, string)
import Data.Maybe           (mapMaybe)
import Data.List     as DL  (reverse, transpose)
import Data.List.Split      (splitOn)
import Control.Monad        (forM_)

type Parser = Parsec () String
type Stacks = Vector (Vector Char)

main :: IO ()
main = do
  [origStacks, origMoves] <- splitOn [""] . lines <$> getContents
  let stripped = transpose $ reverse origStacks
      stacksL  = "" : mapMaybe (parseMaybe ( int *> some upperChar <* space)) strippedg
      stacks   = fromList (fromList . reverse <$> stacksL) -- convert to Vector
      moves    = mapMaybe (parseMaybe ((,,)
                                       <$> (string "move " *> int <* space)
                                       <*> (string "from " *> int <* space)
                                       <*> (string "to "   *> int ))) origMoves
  forM_ [move1, move2] $ \move -> do
    let after = Prelude.foldl move stacks moves
    putStrLn $ toList (head <$> tail after)

move1 :: Stacks -> (Int, Int, Int) -> Stacks
move1 orig (n, from, to)
  | n > 0 = let new = orig // [(to,   cons (head (orig ! from)) (orig ! to))
                              ,(from,       tail (orig ! from))]
            in move1 new (n-1, from, to)
  | otherwise = orig

move2 :: Stacks -> (Int, Int, Int) -> Stacks
move2 orig (n, from, to) =
  orig // [ (to,   take n (orig ! from) <> (orig ! to))
          , (from, drop n (orig ! from)) ]

int :: Parser Int = read <$> some numberChar

