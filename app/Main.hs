module Main (main) where

import System.Environment

import Lib (threadify')

dispatch :: [(String, String -> IO ())]
dispatch = [ ("threadify", threadify')]

main :: IO ()
main = do
    [command, arg] <- getArgs
    let (Just action) = lookup command dispatch
    action arg
