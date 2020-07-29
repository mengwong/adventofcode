#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- here we use parentheses to give familiar comfort, resembling traditional function calls
-- we introduce a lambda, and <$> notation as an alternative to map / fmap

main = do
  input <- getContents
  print(sum( map                                   -- takes two arguments! syntax is lisp-like.
             (\givenMass -> givenMass `div` 3 - 2) -- first argument:  run a lambda function over
             (read <$> lines input)                -- second argument: a list of splitted strings converted to integers
           )
       )


