-- | A module to print help messages
{-# LANGUAGE OverloadedStrings #-}

module Helper (
                help
              ) where

import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , setSGR
  )

-- | A helper to print colored text followed by a newline.
coloredPutStrLn :: Color -> String -> IO ()
coloredPutStrLn color msg = do
  setSGR [SetColor Foreground Vivid color]  -- Turn color on
  putStrLn msg
  setSGR [Reset]                            -- Reset back to default

help :: IO ()
help = do
  putStrLn "Usage: myprog [command] [argument]"
  putStrLn "The following commands are available:"

  coloredPutStrLn Green "  x"
  putStrLn "      Chunkify argument to 280 characters"

  coloredPutStrLn Green "  bsky"
  putStrLn "      Chunkify argument to 300 characters"

  coloredPutStrLn Green "  "
  putStrLn "      (no command) prints this help message."
