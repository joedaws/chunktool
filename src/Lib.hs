module Lib
    ( threadify,
      chunks,
      splitLongWords
    ) where

import Control.Monad
import System.IO
import Control.Arrow (Arrow(second))

-- Each component of a thread has at most 280 characters
threadify :: String -> IO ()
threadify inputString = do
  let validWords = splitLongWords (words inputString) 280
  let c = map unwords $ chunks validWords [] [] 1 280
  printThreads c

printThreads :: [String] -> IO()
printThreads cs = do
  forM_ cs $ \c -> putStr $ c ++ "\n\n"

charCounts :: String -> [(Int, Int)]
charCounts str = scanl (\(index, acc) y -> (index + 1, acc + length y)) (0,0) (words str)

wordLengths :: String -> [(String, Int)]
wordLengths str = map (\s -> (s, length s)) (words str)

-- input is list of words
-- output is list of string chunks
-- spaces count as characters so when checking for length we add in the
-- number of spaces that will be in the final chunk
chunks :: [String] -> [[String]] -> [String] -> Int -> Int -> [[String]]
chunks [] acc [] _ _ = acc
chunks [] acc currentChunk index _ = acc ++ [[show index ++ "-"] ++ currentChunk]
chunks (x:xs) acc currentChunk index limit
  | length x + (sum $ map length currentChunk) + length currentChunk - 1 + length acc + (length $ show index) + 2 <= limit = chunks xs acc (currentChunk ++ [x]) index limit
  | otherwise = chunks (x:xs) (acc ++ [[show index ++ "-"] ++ currentChunk]) [] (index+1) limit

-- words must have length less than the chunk limit otherwise
-- chunks will fail
splitLongWords :: [String] -> Int -> [String]
splitLongWords ws limit = reverse $ foldl (split limit) [] ws

split :: Int -> [String] -> String -> [String]
split limit acc w
      | length w <= limit = w:acc
      | otherwise = split limit (h:acc) t
        where (h, t) = splitAt limit w

-- splitWords :: String ->
