module Main (main) where

import System.Environment

import Chunky (chunkify)

dispatch :: [(String, String -> IO ())]
dispatch = [ ("threadify", chunkify)]

main :: IO ()
main = do
    [command, arg] <- getArgs
    let (Just action) = lookup command dispatch
    action arg
