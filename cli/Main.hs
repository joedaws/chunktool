module Main (main) where

import System.Environment

import Chunky (chunkify)

dispatch :: [(String, String -> IO ())]
dispatch = [
  ("x", chunkify 280),
  ("bsky", chunkify 300)]

main :: IO ()
main = do
    [command, arg] <- getArgs
    let (Just action) = lookup command dispatch
    action arg
