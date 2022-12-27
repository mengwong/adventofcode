#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

{-# LANGUAGE TupleSections #-}

module Lib08 where
import qualified Data.List   as DL
import qualified Data.Matrix as DM
import qualified Data.Vector as DV
import           Data.Function ((&))
-- import Control.Monad (forM_)

main :: IO ()
main = do
  input <- getContents
  let nums   = fmap int <$> lines input
      rows   = DL.length nums
      cols   = DL.length (DL.head nums)
      mkMtx  = DM.fromList rows cols . DL.concat
      mtx    = mkMtx nums
      xtm    = DM.transpose mtx -- slightly faster than getCol? maybe?
      visW   = mkMtx (             visibles (-1) <$> (DV.toList              <$> (flip DM.getRow mtx <$> [1 .. rows])))
      visE   = mkMtx (DL.reverse . visibles (-1) <$> (DV.toList . DV.reverse <$> (flip DM.getRow mtx <$> [1 .. rows])))
      visN   = mkMtx (             visibles (-1) <$> (DV.toList              <$> (flip DM.getRow xtm <$> [1 .. cols]))) & DM.transpose
      visS   = mkMtx (DL.reverse . visibles (-1) <$> (DV.toList . DV.reverse <$> (flip DM.getRow xtm <$> [1 .. cols]))) & DM.transpose
      visAll = visW <+> visE <+> visN <+> visS
  -- forM_ (fmap showI <$> ((,True) <$> mtx) : [visW, visE, visN, visS, visAll]) $ \m -> putStrLn (DM.prettyMatrix m)

  -- part 1: how many trees are visible?
  print $ length (filter snd $ DM.toList visAll)

  -- part 2: what's the maximum scenic score?
  print $ maximum $ DM.toList $ DM.matrix rows cols $ uncurry (scenicScore mtx)

  where
    int :: Char -> Int
    int = read . (:[])
    showI :: (Int,Bool) -> Char
    showI (i,b) = if b then head (show i) else ' '

-- * Part 1

-- | For any two given directions, a tree is visible if it is visible from either direction.
-- Union the matrices based on that binop.
(<+>) :: DM.Matrix (a,Bool) -> DM.Matrix (a,Bool) -> DM.Matrix (a,Bool)
(<+>) = DM.elementwise (\(x1,b1) (x2,b2) -> (x1, b1 || b2))

-- | mark certain trees as visible from a given direction
visibles :: (Ord a) => a -> [a] -> [(a,Bool)]
visibles _ [] = []
visibles threshold (x:xs)
  | x > threshold = (x,True)  : visibles x         xs
  | otherwise     = (x,False) : visibles threshold xs

-- * Part 2

-- | from a given starting point, compute the scenic score
scenicScore :: DM.Matrix Int -> Int -> Int -> Int
scenicScore mtx y x =
  let myheight =   DM.getElem y  x  mtx
      lookN    = [ DM.getElem y' x  mtx | y' <- [y-1, y-2 ..            1 ] ]
      lookS    = [ DM.getElem y' x  mtx | y' <- [y+1, y+2 .. DM.nrows mtx ] ]
      lookW    = [ DM.getElem y  x' mtx | x' <- [x-1, x-2 ..            1 ] ]
      lookE    = [ DM.getElem y  x' mtx | x' <- [x+1, x+2 .. DM.ncols mtx ] ]
  in foldl1 (*) $ go myheight 0 <$> [lookN, lookS, lookW, lookE]
  where
    go :: (Ord a) => a -> Int -> [a] -> Int
    go me d [] = d
    go me d (x:xs)
      | x <  me = go me (d+1) xs
      | x >= me =        d+1
