#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

import Prelude hiding (head, tail, drop, take)
import Text.Megaparsec ( parseMaybe, many, some, sepBy1, Parsec, eof, (<|>) )
import Text.Megaparsec.Char ( char, numberChar, upperChar, space, eol, string )
import Data.Maybe (fromMaybe, mapMaybe)
import           Data.Vector as Vec hiding (mapMaybe, forM_)
import qualified Data.List as DL
import Control.Monad (forM_)
import Data.List.Split (splitOn)

type Parser = Parsec () String

type Stacks = Vec.Vector (Vec.Vector Char)

-- for full theoretical treatment, see Allen's Interval Algebra
main :: IO ()
main = do
  [origStacks, origMoves] <- splitOn [""] <$> lines <$> getContents
  let stripped = DL.transpose $ DL.reverse $ origStacks
      stacksL = "" : (DL.reverse <$> mapMaybe (parseMaybe ( int *> some upperChar <* space)) stripped)
      stacks = fromList (fromList <$> stacksL) -- convert to Vector
  let moves = mapMaybe (parseMaybe ((,,)
                                     <$> (string "move " *> int <* space)
                                     <*> (string "from " *> int <* space)
                                     <*> (string "to "   *> int ))) origMoves
  forM_ [move1, move2] $ \move -> do
    let after = Prelude.foldl move stacks moves
    putStrLn $ Vec.toList (head <$> tail after)

int :: Parser Int
int = read <$> some numberChar

move1 :: Stacks -> (Int, Int, Int) -> Stacks
move1 orig (n, from, to)
  | n > 0 = let new = orig // [(to,   cons (head (orig ! from)) (orig ! to))
                              ,(from,       tail (orig ! from))]
            in move1 new (n-1, from, to)
  | otherwise = orig

move2 :: Stacks -> (Int, Int, Int) -> Stacks
move2 orig (n, from, to) =
  orig // [(to,   take n (orig ! from) <> (orig ! to))
          ,(from, drop n (orig ! from))]


