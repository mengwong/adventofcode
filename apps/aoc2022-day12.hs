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

-- we construct a graph of character nodes, and height-delta edges
type HeightGr = Gr   -- FGL graph
                Char -- ^ character
                Int  -- ^ height delta

main :: IO ()
main = do
  input <- DM.fromLists . lines <$> getContents
  putStrLn $ DM.prettyMatrix input
  -- construct an FGL graph of permissible moves, based on climb capability: max one level up, jump any number of levels down
  let gr :: HeightGr
      gr = buildGr [ ( [], nodeId, myelem
                     , filter ((<=1) . fst) $ catMaybes (outN ++ outE ++ outS ++ outW) ) -- reachable edges have delta <= 1
                   | (nodeId,myelem) <- zip [0..] (DM.toList input)
                   , let (row, col) = nodeNtoRowCol input nodeId
                         mychar = sex myelem
                         -- compute the height deltas relative to the current cell
                         outN = [ (,go N input (row,col)) <$> d | let d = subtract mychar . sex <$> getn N input (row,col) ]
                         outS = [ (,go S input (row,col)) <$> d | let d = subtract mychar . sex <$> getn S input (row,col) ]
                         outE = [ (,go E input (row,col)) <$> d | let d = subtract mychar . sex <$> getn E input (row,col) ]
                         outW = [ (,go W input (row,col)) <$> d | let d = subtract mychar . sex <$> getn W input (row,col) ]
                   ]
      -- the start and end nodes are labeled S and E
      sNode = fromJust $ elemIndex 'S' (DM.toList input)
      eNode = fromJust $ elemIndex 'E' (DM.toList input)
      -- given a certain start node, find a path based on edge length (not edge weight, which would call for Dijkstra's)
      solve s = esp s eNode gr
      path = solve sNode
  putStrLn (asArrows input gr path)
  putStrLn $ "part 1: shortest path length = " ++ show (length path - 1)

--  the brute force approach is something like n^2
--  let possibleStartNodes = [ n | (n,c) <- zip [0..] (DM.toList input), c == 'a' ]
--      solutions = sortOn length $ filter (not . null) $ solve <$> possibleStartNodes
--  putStrLn $ "part 2 (brute): ideal starting point has " ++ show (length (head solutions) - 1) ++ " steps"

  -- the smart approach reconceives the end node as the root of a tree, and we BFS the tree for an 'a' node
  let reversed = mkGraph (labNodes gr) ((\(a,b,c) -> (b,a,c)) <$> labEdges gr) :: HeightGr
      asVector = DM.getMatrixAsVector input
      nearest = sortOn snd $ filter (\(n, _level) -> asVector DV.! n == 'a') $ level eNode reversed
  putStrLn $ "part 2: shortest path to any 'a' has " ++ show (snd (head nearest)) ++ " steps"

-- | the Start and End nodes are valued 'a' and 'z'
sex :: Char -> Int
sex 'S' = ord 'a'
sex 'E' = ord 'z'
sex  x  = ord  x

-- | show the arrows to match the original problem
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

-- | we use compass directions to refer to neighbours
data Compass = N | E | S | W
  deriving (Eq, Show)

-- | convert Node ID to (row,col)
nodeNtoRowCol :: DM.Matrix a -> Int -> (Int, Int)
nodeNtoRowCol input nodeId =
  let row = nodeId `div` DM.ncols input + 1
      col = nodeId `mod` DM.ncols input + 1 
  in ( row, col )

-- | convert (row,col) to Node ID
rowColToNodeN :: DM.Matrix a -> (Int, Int) -> Int
rowColToNodeN input (row, col) = (row - 1) * DM.ncols input + col - 1

-- | get the character of the neighbouring element, if there is one -- we might be at the border!
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

-- | return the node ID of the neighbour
go :: Compass -> DM.Matrix a -> (Int, Int) -> Int
go N input (row,col) = rowColToNodeN input (row-1, col+0)
go S input (row,col) = rowColToNodeN input (row+1, col+0)
go E input (row,col) = rowColToNodeN input (row+0, col+1)
go W input (row,col) = rowColToNodeN input (row+0, col-1)



