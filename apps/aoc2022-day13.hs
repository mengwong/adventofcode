{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text   as T
import Data.Maybe ( fromJust )
import Data.List ( elemIndex, sort, intercalate )
import Data.List.Split ( chunksOf )
import Text.Megaparsec
    ( (<|>), parseMaybe, between, sepBy, some, Parsec )
import Text.Megaparsec.Char ( digitChar )

type Parser = Parsec () String

data Nested = I Int | N [Nested]
  deriving (Eq)

instance Ord Nested where
  compare (I l)    (I r)    = compare l r
  compare (N [])   (N [])   = EQ
  compare (N [])   (N _)    = LT
  compare (N _)    (N [])   = GT
  compare (N (l:ls)) (N (r:rs)) = case l `compare` r of
                                    LT -> LT
                                    EQ -> compare (N ls) (N rs)
                                    GT -> GT
  compare (I l)    (N rs)   = compare (N [I l]) (N rs)
  compare (N ls)   (I r )   = compare (N ls) (N [I r])

instance Show Nested where
  show (I n)  = show n
  show (N ns) = "[" ++ intercalate "," (show <$> ns) ++ "]"

pNested :: Parser [Nested]
pNested = between "[" "]" (pInner `sepBy` ",")
  where
    pInner = I <$> pInt <|>
             N <$> pNested
    pInt :: Parser Int
    pInt = read <$> some digitChar

main :: IO ()
main = do
  input <- fmap (fromJust . parseMaybe pNested)
           . filter (not . null)
           . lines <$> getContents

  let results = zip [1..] [ answer $ compare l r
                          | (l,r) <- fmap (\[a,b] -> (a, b))
                            . chunksOf 2
                            $ input
                          ]
      n2 = [N [N [I 2]]]
      n6 = [N [N [I 6]]]
      part2 = sort $ n2 : n6 : input

  print $ sum $ fst <$> filter snd results
  -- putStrLn $ unlines $ show <$> part2

  print ( maybe 0 (+1) (n2 `elemIndex` part2)
        * maybe 0 (+1) (n6 `elemIndex` part2) )

  where answer LT = True
        answer GT = False
