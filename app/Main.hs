module Main (main) where

import Lib (threadify)

main :: IO ()
main = do
  threadify "hello.txt"
