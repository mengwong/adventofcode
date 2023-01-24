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

data Monkey = M { num      :: Int
                , op       :: MathLang
                , test     :: Int
                , destT    :: Int
                , destF    :: Int
                }
              deriving (Eq, Show)

-- which monkey is holding itmes at the start of which round?
data MonkeyState = MS { holds        :: [Integer]
                      , inspectCount :: Integer
                      }
  deriving Show

-- roundCount = 2000
roundCount = 10000

main :: IO ()
main = do
  input <- getContents
  case runParser traceP "day11" input of
    Left x  -> fail $ errorBundlePretty x
    Right x -> do
      putStrLn "parse successful!"
      
      let (monkeys, starting) = unzip x

      dumpCallGraph monkeys
      putStrLn "* Math"
      let chinese = toInteger $ product (test <$> monkeys)
      putStrLn $ "the product of all the divisors is " ++ show chinese
      
      putStrLn "* Run Log"

      finalState <- execStateT (foldM (const $ runRound chinese monkeys) () [1..roundCount]) (DV.fromList starting)
      let monkeyBusiness = product (take 2 (reverse $ sort ( inspectCount <$> DV.toList finalState )))
      putStrLn $ "* monkey business = " ++ show monkeyBusiness
      return ()

dumpCallGraph :: [Monkey] -> IO ()
dumpCallGraph xs = do
  putStrLn "* Monkey Call Graph"
  putStrLn "#+begin_src dot :tangle monkeys.dot"
  putStrLn "digraph G {"
  mapM_ dumpMonkey xs
  putStrLn "}"
  putStrLn "#+end_src"

dumpMonkey :: Monkey -> IO ()
dumpMonkey monkey = do
  putStrLn $ "M" ++ show monkey.num                                                   ++ " [label=\"M" ++ show monkey.num ++ "\\n" ++ dumpOp monkey.op ++ "\"]"
  putStrLn $ "M" ++ show monkey.num ++ ":se" ++ " -> " ++ "M" ++ show monkey.destT ++ ":nw" ++ " [label=" ++ show monkey.test ++ "]"
  putStrLn $ "M" ++ show monkey.num ++ ":sw" ++ " -> " ++ "M" ++ show monkey.destF ++ ":ne"
  putStrLn ""
  where
    dumpOp MOld = "old"
    dumpOp (ML m) = show m
    dumpOp (MTimes mx my) = dumpOp mx ++ " * " ++ dumpOp my
    dumpOp (MPlus  mx my) = dumpOp mx ++ " + " ++ dumpOp my

mlEval n  MOld  = n
mlEval n (ML m) = m
mlEval n (MTimes mx my) = mlEval n mx * mlEval n my
mlEval n (MPlus  mx my) = mlEval n mx + mlEval n my
    
runMonkey :: Integer -> Bool -> Monkey -> StateT (DV.Vector MonkeyState) IO ()
runMonkey chinese verbose monkey = do
  startState <- get
  when verbose $ liftIO $ putStrLn ("*** Monkey " ++ show monkey.num ++ ":")
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
        let worryHigh = mlEval itemNum monkey.op
            worryLow = worryHigh `mod` chinese
            -- worryLow = worryHigh `div` 1
        when (verbose) $ do
          liftIO $ putStrLn ("**** Monkey " ++ show monkey.num ++ " inspects an item with an initial worry level of " ++ show itemNum)
          liftIO $ putStrLn ("    new worry level = " ++ show worryHigh)
          if worryHigh /= worryLow
            then liftIO $ putStrLn ("    Monkey gets bored with item. Worry level is mod chinese to " ++ show worryLow)
            else liftIO $ putStrLn ("    Monkey gets bored with item. Worry level unchanged.")
        if fromIntegral worryLow `mod` monkey.test == 0
          then do
          when verbose $ liftIO $ putStrLn ("    Item with worry level " ++ show worryLow ++ " is thrown to monkey " ++ show monkey.destT)
          monkeyTakes monkey.destT worryLow
          else do
          when verbose $ liftIO $ putStrLn ("    Item with worry level " ++ show worryLow ++ " is thrown to monkey " ++ show monkey.destF)
          monkeyTakes monkey.destF worryLow
        return ()

        where monkeyTakes :: Int -> Integer -> StateT (DV.Vector MonkeyState) IO ()
              monkeyTakes destM itemN =
                modify (\st -> st DV.// pure (destM, let oldM = st DV.! destM
                                                     in  oldM { holds = oldM.holds ++ [itemN] }))
      monkeyInspects :: Int -> Integer -> StateT (DV.Vector MonkeyState) IO ()
      monkeyInspects mNum increments =
        modify (\st -> st DV.// pure (mNum, let oldM = st DV.! mNum
                                            in  oldM { inspectCount = oldM.inspectCount + increments }))
      
runRound :: Integer -> [Monkey] -> Int -> StateT (DV.Vector MonkeyState) IO ()
runRound chinese monkeys roundN = do
  let verbose = roundN `mod` 1000 == 0
  when verbose $ liftIO . putStrLn $ "** round " ++ show roundN
  mapM_ (runMonkey chinese verbose) monkeys
  endState <- get
  when verbose $ liftIO $ putStrLn $ "*** at the end of round " ++ show roundN ++ ", the monkeys have counted this many inspections: " ++ show (inspectCount <$> DV.toList endState)
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

  let 
      m  = M (fromIntegral mN) mO mT (fromIntegral m1) (fromIntegral m2)
      ms = MS mI 0

  return (m, ms)

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

testP :: Parser Int
testP = "divisible by " *> (fromIntegral <$> pInt)

pInt :: Parser Integer
pInt = read <$> some digitChar


-- * Math Matters
--
-- The missing clue is this: we can exploit the Chinese Remainder Theorem.
--
-- The divisors are coprime.
--
-- So we construct the product of divisors, and after each monkey.op transformation, we mod N and use the remainder.

