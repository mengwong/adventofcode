#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- yes you can debug trace
import Debug.Trace
main = do
  input <- getContents                 
  let mylines = lines input            
      modules = map read mylines
      fuels   = map fuelNeeded (traceShow modules modules)
      total   = sum            (traceShow fuels fuels)
  print total                          

fuelNeeded mass = mass `div` 3 - 2     


