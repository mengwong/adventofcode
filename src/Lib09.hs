{-# LANGUAGE TupleSections #-}

module Lib09 where
import qualified Data.List   as DL
import Control.Monad.Trans.State

data Dir = U | D | L | R              deriving (Eq, Show, Read)
data Point = P { x :: Int, y :: Int } deriving (Eq, Show)
type Rope = [Point]

main :: IO ()
main = do
  input <- lines <$> getContents
  let moves = concat [ replicate (read nsteps :: Int) (read dir :: Dir)
                     | i <- input , let [dir, nsteps] = words i ]
  print $ (\ropeLen -> length $ DL.nub (last <$> DL.scanl move (replicate ropeLen (P 0 0)) moves)) <$> [2,10]
  
move :: Rope -> Dir -> Rope -- | move the rope
move r d = let n = lead (head r) d in n : snd (DL.mapAccumL (\h t -> (step (t,h), step (t,h))) n (tail r))

lead :: Point -> Dir -> Point -- | move the head
lead p U = p <+> P ( 0) (-1)
lead p D = p <+> P ( 0) ( 1)
lead p L = p <+> P (-1) ( 0)
lead p R = p <+> P ( 1) ( 0)
  
step :: (Point,Point) -> Point -- | move the tail
step (t,h) = t <+> follow t h

follow :: Point -> Point -> Point -- | relatively, where does the tail go?
follow t h = let P x y = t <-> h in if 2 `elem` [abs x, abs y] then P (signum x) (signum y) else P 0 0

-- | from the point of view of the tail, how far away is the head?
(<->) :: Point -> Point -> Point
(<->) t h = P (x h - x t) (y h - y t)

-- | move a point by some relative distance
(<+>) :: Point -> Point -> Point
(<+>) p n = P (x p + x n) (y p + y n)

