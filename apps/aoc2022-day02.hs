#!/usr/bin/env stack
-- stack --resolver lts-19.33 script

module Main where
data UPlay = A | B | C deriving (Show, Read, Eq, Enum)
data IPlay = X | Y | Z deriving (Show, Read, Eq, Enum, Bounded)
data  Play = R | P | S deriving (Show, Read, Eq, Enum)

uplay :: UPlay -> Play
uplay A = R
uplay B = P
uplay C = S

iplay :: IPlay -> Play
iplay X = R
iplay Y = P
iplay Z = S

beats :: Play -> Play -> Bool
R `beats` S = True
P `beats` R = True
S `beats` P = True
_ `beats` _ = False

data Outcome = Win | Lose | Draw deriving (Show, Read, Eq, Enum)

outcome :: Play -> Play -> Outcome
outcome u i
  | i `beats` u = Win
  | u `beats` i = Lose
  | otherwise   = Draw

quantify :: Num a => Outcome -> a
quantify Win  = 6
quantify Lose = 0
quantify Draw = 3

shape :: Play -> Int
shape R = 1
shape P = 2
shape S = 3

score :: UPlay -> IPlay -> Int
score u i = quantify (outcome u' i') + shape i'
  where u' = uplay u
        i' = iplay i

main :: IO ()
main = do
  input <- lines <$> getContents
  let rounds = [ (u,i, score u i)
               | line <- input
               , let [u',i'] = words line -- wow this is so fragile
                     u = read u'
                     i = read i'
               ]
  -- putStr $ unlines $ show <$> rounds
  putStrLn $ "part 1: " ++ show (sum [ s | (_,_,s) <- rounds ])
  let part2 = [ (u,o,i, score u i)
              | (u,o,_) <- rounds
              , let i = brute u (desiredResult o)
              ]
  -- putStr $ unlines $ show <$> part2
  putStrLn $ "part 2: " ++ show (sum [ s | (_,_,_,s) <- part2 ])

desiredResult :: IPlay -> Outcome
desiredResult X = Lose
desiredResult Y = Draw
desiredResult Z = Win

-- we could brute-force our way to the answer using backward-chaining
brute, smart :: UPlay -> Outcome -> IPlay
brute u o = head [ i
                 | i <- [ minBound .. maxBound ]
                 , uplay u `outcome` iplay i == o
                 ]

-- or we could be smart about it because things are pretty well determined
smart u o = case smart' (uplay u) o of
              R -> X
              P -> Y
              S -> Z

smart' :: Play -> Outcome -> Play
smart' R  Win = P
smart' R Lose = S
smart' P  Win = S
smart' P Lose = R
smart' S  Win = R
smart' S Lose = P
smart' x Draw = x

