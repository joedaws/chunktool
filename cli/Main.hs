module Main (main) where

import System.Environment

import Helper (help)
import Chunky (chunkify)

data CommandFunction =
  NeedString (String -> IO ())
  | NoString (IO ())

-- | Turn a 'CommandFunction' into a function of type 'String -> IO ()'
--   by ignoring the argument if the command doesn't need one.
runCommand :: CommandFunction -> String -> IO ()
runCommand (NeedString f) userArg = f userArg
runCommand (NoString ioAction)  _ = ioAction

-- | Our dispatch list maps command names to a 'CommandFunction'.
dispatch :: [(String, CommandFunction)]
dispatch =
  [
    ("help", NoString help)
  , ("x"   , NeedString (chunkify 280))
  , ("bsky", NeedString (chunkify 300))
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- 0 arguments: Just run help.
    [] -> help

    -- 1 arguments: Just run help also but open the possibility of more.
    [command] -> do
      let maybeCmdFunction = lookup command dispatch
      case maybeCmdFunction of
        Nothing -> do
          putStrLn ("Unrecognized command: " <> command)
          help
        Just cmdFunction -> runCommand cmdFunction ""

    -- 2 arguments: interpret them as command and arg
    [command, arg] -> do
      let maybeCmdFunction = lookup command dispatch
      case maybeCmdFunction of
        Nothing -> do
          putStrLn ("Unrecognized command: " <> command)
          help
        Just cmdFunction -> runCommand cmdFunction arg

    -- Anything else is invalid, show help.
    _ -> do
      putStrLn "Invalid number of arguments."
      help
