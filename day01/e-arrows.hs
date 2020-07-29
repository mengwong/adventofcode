#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
import Control.Arrow
main = getContents >>=
  (lines
   >>> fmap read
   >>> fmap ((`div`3)>>>(subtract 2))
   >>> sum
   >>> print)
  

