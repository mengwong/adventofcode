{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Vector  as DV
import qualified Data.Matrix  as DM
import Data.Maybe ( catMaybes, fromJust )
import Data.List ( elemIndex, sortOn )
import Data.Char ( ord )
import Data.Graph.Inductive ( buildGr, mkGraph, esp, Gr, level, emap, labNodes, labEdges )
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  input <- DM.fromLists . lines <$> getContents
  putStrLn $ DM.prettyMatrix input
  let gr :: HeightGr
      gr = buildGr [ ( [], nodeId, myelem, filter ((<=1) . fst) $ catMaybes (outN ++ outE ++ outS ++ outW) )
                   | (nodeId,myelem) <- zip [0..] (DM.toList input)
                   , let (row, col) = nodeNtoRowCol input nodeId
                         mychar = sex myelem
                         outN = [ (,go N input (row,col)) <$> d | let d = (\x -> x - mychar) . sex <$> getn N input (row,col) ]
                         outS = [ (,go S input (row,col)) <$> d | let d = (\x -> x - mychar) . sex <$> getn S input (row,col) ]
                         outE = [ (,go E input (row,col)) <$> d | let d = (\x -> x - mychar) . sex <$> getn E input (row,col) ]
                         outW = [ (,go W input (row,col)) <$> d | let d = (\x -> x - mychar) . sex <$> getn W input (row,col) ]
                   ]
      sNode = fromJust $ elemIndex 'S' (DM.toList input)
      eNode = fromJust $ elemIndex 'E' (DM.toList input)
      solve s = esp s eNode gr
      path = solve sNode
  putStrLn (asArrows input gr path)
  putStrLn $ "part 1: shortest path length = " ++ show (length path - 1)

--  let possibleStartNodes = [ n | (n,c) <- zip [0..] (DM.toList input), c == 'a' ]
--      solutions = sortOn length $ filter (not . null) $ solve <$> possibleStartNodes
--  putStrLn $ "part 2 (brute): ideal starting point has " ++ show (length (head solutions) - 1) ++ " steps"

  let reversed = mkGraph (labNodes gr) ((\(a,b,c) -> (b,a,c)) <$> labEdges gr) :: HeightGr
      asVector = DM.getMatrixAsVector input
      nearest = sortOn snd $ filter (\(n, _level) -> asVector DV.! n == 'a') $ level eNode reversed
  putStrLn $ "part 2: shortest path to any 'a' has " ++ show (snd (head nearest)) ++ " steps"

sex :: Char -> Int
sex 'S' = ord 'a'
sex 'E' = ord 'z'
sex  x  = ord  x

asArrows :: DM.Matrix Char -> HeightGr -> [Int] -> String
asArrows mtx gr path =
  let blank = DM.mapPos (\_ _ -> '.') mtx
      arrowed = foldl (\m f -> f m) blank [ DM.setElem arrow (row,col)
                                          | (node,next) <- zip path (drop 1 path ++ [0])
                                          , let (row, col) = nodeNtoRowCol mtx node
                                                arrow = if next == 0
                                                        then 'E'
                                                        else dir (nodeNtoRowCol mtx node) (nodeNtoRowCol mtx  next)
                                          ]
      origs = foldl (\m f -> f m) blank [ DM.setElem origChar (row,col)
                                        | (node,next) <- zip path (drop 1 path ++ [0])
                                        , let (row, col) = nodeNtoRowCol mtx node
                                              origChar = DM.getElem row col mtx 
                                        ]
  in unlines (DM.toLists arrowed ++ [""] ++ DM.toLists origs)
  where
    dir (y1,x1) (y2,x2)
      | y2 > y1 = 'v'
      | y2 < y1 = '^'
      | x2 > x1 = '>'
      | x2 < x1 = '<'
      | otherwise = ' '

data Compass = N | E | S | W
  deriving (Eq, Show)

nodeNtoRowCol :: DM.Matrix a -> Int -> (Int, Int)
nodeNtoRowCol input nodeId =
  let row = nodeId `div` DM.ncols input + 1
      col = nodeId `mod` DM.ncols input + 1 
  in ( row, col )

rowColToNodeN :: DM.Matrix a -> (Int, Int) -> Int
rowColToNodeN input (row, col) = (row - 1) * DM.ncols input + col - 1

getn :: Compass -> DM.Matrix a -> (Int, Int) -> Maybe a
getn c input (row,col) =
  case c of
    N -> inbounds $ DM.getElem (row-1) (col+0) input
    S -> inbounds $ DM.getElem (row+1) (col+0) input
    E -> inbounds $ DM.getElem (row+0) (col+1) input
    W -> inbounds $ DM.getElem (row+0) (col-1) input
  where
    inbounds :: a -> Maybe a
    inbounds
      | c == N , row > 1              = Just
      | c == W , col > 1              = Just
      | c == S , row < DM.nrows input = Just
      | c == E , col < DM.ncols input = Just
      | otherwise                     = const Nothing

go :: Compass -> DM.Matrix a -> (Int, Int) -> Int
go N input (row,col) = rowColToNodeN input (row-1, col+0)
go S input (row,col) = rowColToNodeN input (row+1, col+0)
go E input (row,col) = rowColToNodeN input (row+0, col+1)
go W input (row,col) = rowColToNodeN input (row+0, col-1)

type HeightGr = Gr
                Char -- ^ character
                Int  -- ^ height delta

pInt :: String -> Integer
pInt = read


