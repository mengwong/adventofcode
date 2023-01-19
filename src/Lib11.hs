{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}


module Lib11 where
import qualified Data.Map as Map
import qualified Data.Vector  as DV
import qualified Data.Matrix  as DM
import Text.Megaparsec        (runParser, many, some, Parsec, choice , try, errorBundlePretty, anySingleBut, (<?>), (<|>), satisfy, anySingle, sepBy)
import Text.Megaparsec.Char   (char, string, alphaNumChar, punctuationChar, hspace1, hspace, space, digitChar, eol, spaceChar, newline)
import Text.Parsec.Combinator (manyTill, eof)
import Control.Monad.Combinators.Expr
import Data.Maybe             (mapMaybe, fromMaybe, listToMaybe)
import Data.List    (reverse, transpose, foldl', nub, intercalate, partition, sort)
import Data.Either
import Data.Void
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List.Split (splitOn)
import qualified Data.Tree as Tree

-- | We construct a couple of progressively correct representations of the filesystem.
-- First, we parse to a Map of path to files.
-- Second, we convert the Map to a Tree.
-- Then we solve part a and b by querying against the Tree.

data Monkey = M { num      :: Int
                , op       :: Integer -> Integer
                , test     :: Integer -> Bool -- usually a mod op but who knows what Part 2 will bring?
                , destT    :: Int
                , destF    :: Int
                }

-- which monkey is holding itmes at the start of which round?
data MonkeyState = MS { holds        :: [Integer]
                      , inspectCount :: Integer
                      }
  deriving Show

main :: IO ()
main = do
  input <- getContents
  case runParser traceP "day11" input of
    Left x  -> fail $ errorBundlePretty x
    Right x -> do
      putStrLn "parse successful!"
      
      let roundCount = 10000
          (monkeys, starting) = unzip x
      finalState <- execStateT (foldM (const $ runRound monkeys) () [1..roundCount]) (DV.fromList starting)
      let monkeyBusiness = product (take 2 (reverse $ sort ( inspectCount <$> DV.toList finalState )))
      putStrLn $ "monkey business = " ++ show monkeyBusiness
      return ()
      
runMonkey :: Monkey -> StateT (DV.Vector MonkeyState) IO ()
runMonkey monkey = do
  startState <- get
  -- liftIO $ putStrLn ("Monkey " ++ show monkey.num ++ ":")
  let heldItems = holds (startState DV.! monkey.num)
  mapM runItem heldItems
  monkeyInspects monkey.num (fromIntegral $ length heldItems)

  return ()
    where
      runItem :: Integer -> StateT (DV.Vector MonkeyState) IO ()
      runItem itemNum = do
        -- we are now inspecting it, so we take it out of the hold
        modify (\st -> st DV.// pure (monkey.num, let oldM = st DV.! monkey.num
                                                  in  oldM { holds = drop 1 oldM.holds }))
        let becomes = monkey.op itemNum
         -- div3 = becomes `div` 3
            div3 = becomes
--        liftIO $ putStrLn ("  Monkey inspects an item with a worry level of " ++ show itemNum)
--        liftIO $ putStrLn ("    Worry level becomes " ++ show becomes)
--        liftIO $ putStrLn ("    Monkey gets bored with item. Worry level is not divided by 3 to " ++ show div3)
        if monkey.test div3
          then do
--          liftIO $ putStrLn ("    Item with worry level " ++ show div3 ++ " is thrown to monkey " ++ show monkey.destT)
          monkeyTakes monkey.destT div3
          else do
--          liftIO $ putStrLn ("    Item with worry level " ++ show div3 ++ " is thrown to monkey " ++ show monkey.destF)
          monkeyTakes monkey.destF div3
        return ()

        where monkeyTakes :: Int -> Integer -> StateT (DV.Vector MonkeyState) IO ()
              monkeyTakes destM itemN =
                modify (\st -> st DV.// pure (destM, let oldM = st DV.! destM
                                                     in  oldM { holds = oldM.holds ++ [itemN] }))
      monkeyInspects :: Int -> Integer -> StateT (DV.Vector MonkeyState) IO ()
      monkeyInspects mNum increments =
        modify (\st -> st DV.// pure (mNum, let oldM = st DV.! mNum
                                            in  oldM { inspectCount = oldM.inspectCount + increments }))
      
runRound :: [Monkey] -> Int -> StateT (DV.Vector MonkeyState) IO ()
runRound monkeys roundN = do
  mapM_ runMonkey monkeys
  endState <- get
  liftIO $ putStrLn $ "at the end of round " ++ show roundN ++ ", the monkeys have counted this many inspections:"
  liftIO $ print (inspectCount <$> DV.toList endState)
  return ()
    
type Parser     = Parsec Void String                 -- ^ your basic megaparsec

type Rounds = [(Monkey, MonkeyState)]
  
traceP :: Parser Rounds
traceP = some (stanzaP <* many eol)

stanzaP :: Parser (Monkey, MonkeyState)
stanzaP = do
  mN <- "Monkey "                               *> pInt     <* ":" <* eol
  mI <- hspace1 *> "Starting items: "           *> sepBy pInt ", " <* eol
  mO <- hspace1 *> "Operation: new = "          *> mathP           <* eol
  mT <- hspace1 *> "Test: "                     *> testP           <* eol
  m1 <- hspace1 *> "If true: throw to monkey "  *> pInt            <* eol
  m2 <- hspace1 *> "If false: throw to monkey " *> pInt            <* eol

  let mE = \n -> mlEval n mO
      m  = M (fromIntegral mN) mE mT (fromIntegral m1) (fromIntegral m2)
      ms = MS mI 0

  return (m, ms)

  where
    mlEval n  MOld  = n
    mlEval n (ML m) = m
    mlEval n (MTimes mx my) = mlEval n mx * mlEval n my
    mlEval n (MPlus  mx my) = mlEval n mx + mlEval n my
    

data MathLang = ML Integer
              | MOld
              | MTimes MathLang MathLang
              | MPlus  MathLang MathLang
              deriving (Eq, Show)

mathP :: Parser MathLang
mathP = makeExprParser term table <?> "mathLang expression parser"
  where -- in future, add lexing so we don't have to hspace everywhere
    term = (ML <$> (try (hspace *> pInt))) <|> (MOld <$ (try (hspace *> "old"))) <?> "term"
    table = [ [ binary (try (hspace *> "*")) MTimes ]
            , [ binary (try (hspace *> "+")) MPlus ] ]
    binary name f = InfixL (f <$ name)

testP :: Parser (Integer -> Bool)
testP = do
  mD <- "divisible by " *> pInt
  return (\n -> n `mod` mD == 0)


pInt :: Parser Integer
pInt = read <$> some digitChar

