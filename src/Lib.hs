module Lib
    ( threadify,
      chunks
    ) where

import System.IO

-- Each component of a thread has at most 280 characters
threadify :: String -> IO ()
threadify fileName = do
    withFile fileName ReadMode (\handle ->
        do
            contents <- hGetContents handle
            let w = words contents
            let c = map unwords $ chunks w [] [] 280
            putStr $ show c)

charCounts :: String -> [(Int, Int)]
charCounts str = scanl (\(index, acc) y -> (index + 1, acc + length y)) (0,0) (words str)

wordLengths :: String -> [(String, Int)]
wordLengths str = map (\s -> (s, length s)) (words str)

-- input is list of words
-- output is list of string chunks
-- spaces count as characters so when checking for length we add in the
-- number of spaces that will be in the final chunk
chunks :: [String] -> [[String]] -> [String] -> Int -> [[String]]
chunks [] acc [] _ = acc
chunks [] acc currentChunk _ = acc ++ [currentChunk]
chunks (x:xs) acc currentChunk limit
  | length x + (sum $ map length currentChunk) + length currentChunk - 1 + length acc <= limit = chunks xs acc (currentChunk ++ [x]) limit
  | otherwise = chunks (x:xs) (acc ++ [currentChunk]) [] limit
