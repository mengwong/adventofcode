#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

module Main where

import Prelude       hiding (head, tail, drop, take)
import Data.Vector   hiding (mapMaybe, forM_, reverse, foldl')
import Text.Megaparsec      (parseMaybe, many, some, Parsec)
import Text.Megaparsec.Char (char, numberChar, upperChar, space, string)
import Data.Maybe           (mapMaybe, fromMaybe)
import Data.List     as DL  (reverse, transpose, foldl')
import Data.List.Split      (splitOn)
import Control.Monad        (forM_)

type Parser = Parsec () String
type Stacks = Vector (Vector Char)

main :: IO ()
main = do
  [origStacks, origMoves] <- splitOn [""] . lines <$> getContents
  let stripped = mapMaybe (parseMaybe (int *> some upperChar <* space)) (transpose $ reverse origStacks)
      stacks   = fromList (fromList . reverse <$> "" : stripped) -- convert to Vector, rebase to 1-indexed
      moves    = mapMaybe (parseMaybe ((,,)
                                       <$> (string  "move " *> int)
                                       <*> (string " from " *> int)
                                       <*> (string " to "   *> int) )) origMoves
  forM_ [moveBy (Just 1), moveBy Nothing] $ \move -> do
    let after = foldl' move stacks moves
    putStrLn $ toList (head <$> tail after)

moveBy :: Maybe Int -> Stacks -> (Int, Int, Int) -> Stacks
moveBy capacity orig (n, from, to)
  | n > 0 = let new = orig // [(to,   take cap (orig ! from) <> (orig ! to))
                              ,(from, drop cap (orig ! from))]
            in moveBy capacity new (n-cap, from, to)
  | otherwise = orig
  where cap = fromMaybe n capacity

int :: Parser Int
int = read <$> some numberChar
