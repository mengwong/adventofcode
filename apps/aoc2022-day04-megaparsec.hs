#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

import Text.Megaparsec ( parseMaybe, many, some, sepBy1, Parsec )
import Text.Megaparsec.Char ( char, numberChar )
import Data.Maybe (fromMaybe)

type Parser = Parsec () String

-- for full theoretical treatment, see Allen's Interval Algebra
main :: IO ()
main = do
  input <- lines <$> getContents
  let elves = [ (elf1,elf2) :: ([Int],[Int])
              | i <- input
              , let parsed = parseMaybe ( (int `sepBy1` char '-') `sepBy1` char ',' )
                             i
                    [elf1, elf2] = fromMaybe (error "unable to parse input") parsed
              ]
  print $ length $ filter id [ orViceVersa elf1 contains elf2 | (elf1,elf2) <- elves ]
  print $ length $ filter id [ orViceVersa elf1 overlaps elf2 | (elf1,elf2) <- elves ]
  where
    orViceVersa x f y = f x y || f y x

    int :: Parser Int
    int = read <$> some numberChar

contains,overlaps :: (Ord a) => [a] -> [a] -> Bool

a `contains` b = minimum a <= minimum b && maximum b <= maximum a
--                       a------------b============b------------a

a `overlaps` b = minimum a <= minimum b && minimum b <= maximum a
--                       a------------b=========================a=====b
--                       a------------b-------------------------a
