{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions, ParallelListComp #-}

module Main where

import qualified Data.Map     as Map
import qualified Data.Vector  as DV
import qualified Data.Matrix  as DM
import qualified Data.Text   as T
import           Data.Tree
import Data.Maybe ( mapMaybe, fromJust )
import Data.List ( elemIndex, sortOn )
import Data.List.Split ( chunksOf )
import Data.Ord
import Data.Graph.Inductive ( buildGr, mkGraph, esp, Gr, level, emap, labNodes, labEdges, Node )
import Data.Function ((&))
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec () String

data Nested = I Int | N [Nested]
  deriving (Eq, Show)

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

pNested :: Parser [Nested]
pNested = between "[" "]" (pInner `sepBy` ",")
  where
    pInner = I <$> pInt <|>
             N <$> pNested
    pInt :: Parser Int
    pInt = read <$> some digitChar

main :: IO ()
main = do
  input <- fmap (\[a,b] -> (fromJust a, fromJust b))
           . chunksOf 2
           . fmap (parseMaybe pNested)
           . filter (not . null)
           . lines <$> getContents
  print input
  let results = [ (index, answer $ compare l r)
                | (l,r) <- input
                | index <- [1..]]
  print $ sum $ fst <$> filter snd results
  where answer LT = True
        answer GT = False
