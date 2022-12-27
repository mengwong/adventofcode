#!/usr/bin/env stack
-- stack --resolver lts-20.2 script

module Lib07 where
import qualified Data.Map as Map
import qualified Data.Vector  as DV
import Text.Megaparsec        (runParser, many, some, Parsec, choice , try, errorBundlePretty, anySingleBut, (<?>), (<|>), satisfy, anySingle)
import Text.Megaparsec.Char   (char, string, alphaNumChar, punctuationChar, hspace, space, digitChar, eol, spaceChar, newline)
import Text.Parsec.Combinator (manyTill, eof)
import Data.Maybe             (mapMaybe, fromMaybe, listToMaybe)
import Data.List    (reverse, transpose, foldl', nub, intercalate, partition, sort)
import Data.Either
import Data.Void
import Control.Monad
import Control.Monad.Trans.State
import Data.List.Split (splitOn)
import qualified Data.Tree as Tree

-- | We construct a couple of progressively correct representations of the filesystem.
-- First, we parse to a Map of path to files.
-- Second, we convert the Map to a Tree.
-- Then we solve part a and b by querying against the Tree.

main :: IO ()
main = do
  input <- getContents
  case runParser (some traceP) "day07" input of
    Left x  -> fail $ errorBundlePretty x
    Right x -> do
      let fsmap = evalState (buildFS x) [] :: Filesystem
          asDirTree = asTree fsmap []      :: FSTree
      -- part 1: Find all of the directories with a total size of at most 100000.
      -- What is the sum of the total sizes of those directories?
      print $ sum $ [ i
                    | (i,'/':s) <- Tree.flatten asDirTree
                    , i < 100000 ]
      -- part 2
      let totalDisk     = 70000000; neededDisk = 30000000
          currentlyUsed = fst $ Tree.rootLabel asDirTree
          deleteSize    = neededDisk - (totalDisk - currentlyUsed)
      putStrLn $ "looking for the smallest directory larger than " ++ show deleteSize
      print $ head $ sort $ filter (>= deleteSize) (fst <$> Tree.flatten asDirTree)

type CWD        = [String]                           -- ^ path, reversed, so root is to the right
type Filesystem = Map.Map CWD [File]                 -- ^ first pass, we have a flap map of path to files
type FSTree     = Tree.Tree (Int,String)             -- ^ second pass, we have a tree of size and file
data Cmd  = Ls                                       -- ^ "ls"
          | Cd Path            deriving (Eq, Show)   -- ^ "cd Path"
data Path = Root                                     -- ^ "/"
          | Up                                       -- ^ ".."
          | Path String        deriving (Eq, Show)   -- ^ "asdf"
type Trace = ( Cmd                                   -- ^ $ cd or ls
             , [File] )                              -- ^   13234 dgfdf.gf [..]
data File   = DirEnt String                          -- ^ subdirectory
            | Plain Int String deriving (Eq, Show)   -- ^ file with a size
type Parser     = Parsec Void String                 -- ^ your basic megaparsec


-- | parsers for the input strings.
-- we begin by parsing the input into a list of Traces: cd or ls, followed by a list of files and subdirectories.
  
traceP :: Parser Trace
traceP = (,)
         <$> (string "$ " *>
              choice [ Cd <$> (string "cd " *>
                               choice [ Root <$  string "/"
                                      , Up   <$  string ".."
                                      , Path <$> nonWhiteSpace
                                      ])
                     , Ls <$ string "ls" ] <* eol)
         <*> many (choice [ DirEnt <$> (string "dir" *> hspace *> some alphaNumChar)
                          , Plain  <$> pInt <* hspace <*> nonWhiteSpace
                          ] <* eol)

-- | Once we have the parsed command log, we construct the filesystem accordingly.
-- We use a state monad to keep track of CWD.
-- At any point in the trace we have a CWD, which informs how we mutate the filesystem map.

buildFS :: [Trace] -> State CWD Filesystem
buildFS ts = foldM go Map.empty ts
  where
    go :: Filesystem -> Trace -> State CWD Filesystem
    go fs (Cd Root,     _) = put [] >> return fs
    go fs (Cd Up,       _) = do cwd <- get; when (length cwd > 0) $ put (drop 1 cwd); return fs
    go fs (Cd (Path p), _) = do
      cwd <- get
      let ps = reverse (splitOn "/" p)
      put (ps++cwd)
      return fs
    go fs (Ls, files)      = do cwd <- get; return $ Map.union fs (Map.singleton cwd files)

-- | Finally we transform the filesystem map to a proper tree.
-- We do this in a single pass over the map sorted by key (hopefully O(n log n)),
-- "corecursively" generating the tree as the sum of all children at a given depth.
-- for extra credit, try using unfoldTree against the fsmap, though that may be a little slower
asTree :: Filesystem -> CWD -> FSTree
asTree fs cwd =
  let children = go <$> fromMaybe [] (Map.lookup cwd fs)
  in Tree.Node ( sum $ fst . Tree.rootLabel <$> children
               , intercalate "/" ("" : reverse cwd) ) children
  where
    go (DirEnt p)  = asTree fs (p:cwd)
    go (Plain n s) = Tree.Node (n,s) []

-- | utility functions for parsing
nonWhiteSpace :: Parser String
nonWhiteSpace = some (alphaNumChar <|> punctuationChar)

pInt :: Parser Int
pInt = read <$> some digitChar
