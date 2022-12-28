{-# LANGUAGE TupleSections #-}

module Lib09 where
import qualified Data.List   as DL
import qualified Data.Matrix as DM
import qualified Data.Vector as DV
import           Data.Function ((&))
import Control.Monad.Trans.State
-- import Control.Monad (forM_)

data Dir = U | D | L | R -- up, down, left, right
  deriving (Eq, Show, Read)

data Point = P { x :: Int, y :: Int }
  deriving (Eq, Show)

main :: IO ()
main = do
  input <- lines <$> getContents
  let moves = concat [ replicate (read nsteps :: Int) (read dir :: Dir) | i <- input , let [dir, nsteps] = words i ]
      trail = DL.scanl move [P 0 0, P 0 0] moves
  -- print moves
  -- putStrLn $ unlines $ (show <$> trail)
  print $ length $ DL.nub (last <$> trail)
  
type Rope = [Point]

move :: Rope -> Dir -> Rope
move r d = let h' = lead   (head r) d
               t' = (last r) <+> follow (last r) h'
           in [h',t']

lead :: Point -> Dir -> Point
lead p U = p <+> P ( 0) (-1)
lead p D = p <+> P ( 0) ( 1)
lead p L = p <+> P (-1) ( 0)
lead p R = p <+> P ( 1) ( 0)
  
-- | If the head is ever two steps directly up, down, left, or right
-- from the tail, the tail must also move one step in that direction
-- so it remains close enough. Otherwise, if the head and tail aren't
-- touching and aren't in the same row or column, the tail always
-- moves one step diagonally to keep up.

follow :: Point -> Point -> Point
follow t h = case t <-> h of
               P ( 2) ( 0) -> P ( 1) ( 0)
               P (-2) ( 0) -> P (-1) ( 0)
               P ( 0) ( 2) -> P ( 0) ( 1)
               P ( 0) (-2) -> P ( 0) (-1)
               P ( 2) ( 1) -> P ( 1) ( 1)
               P (-2) ( 1) -> P (-1) ( 1)
               P ( 1) ( 2) -> P ( 1) ( 1)
               P ( 1) (-2) -> P ( 1) (-1)
               P ( 2) (-1) -> P ( 1) (-1)
               P (-2) (-1) -> P (-1) (-1)
               P (-1) ( 2) -> P (-1) ( 1)
               P (-1) (-2) -> P (-1) (-1)
               _           -> P   0    0

-- | from the point of view of the tail, how far away is the head?
(<->) :: Point -> Point -> Point
(<->) t h = P (x h - x t) (y h - y t)

-- | move a point by some relative distance
(<+>) :: Point -> Point -> Point
(<+>) p n = P (x p + x n) (y p + y n)

