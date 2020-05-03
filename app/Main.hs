module Main where

import NanoParser

main :: IO ()
main = do
  print $ runS char "a"
  print $ eval $ run "1+2"
  print $ eval $ run "1+2-3"
  print $ eval $ run "1+2-3+4-5-6+3"
  print $ eval $ run "(1+2)"
  print $ eval $ run "1*3"
  print $ eval $ run "1+2*3"
