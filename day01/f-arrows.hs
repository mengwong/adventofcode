#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- let's make up our own operators! we choose $ as an allusion to our old friend <$>

import Control.Arrow
main = getContents >>=
  (lines
   >>>$ read
   >>>$ ((`div`3)>>>(subtract 2))
   >>>  sum
   >>>  print)
  
x >>>$ y = x >>> fmap y
