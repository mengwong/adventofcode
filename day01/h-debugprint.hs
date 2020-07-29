#!/usr/bin/env stack
-- stack --resolver lts-16.2 script
-- a saner style of printing
import Debug.Trace
main = do
  input <- getContents                 
  let mylines = lines input
  putStrLn $ "input = " ++ unwords mylines

  let modules = map read mylines
  putStrLn $ "modules = " ++ show modules

  let fuels   = map fuelNeeded modules
  putStrLn $ "fuels = " ++ show fuels

  let total   = sum fuels
  print total                          

fuelNeeded mass = mass `div` 3 - 2     


