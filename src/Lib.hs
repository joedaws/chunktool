module Lib
    ( threadify,
      threadify',
      chunks,
      splitLongWords
    ) where

import Control.Monad

-- Each component of a thread has at most 280 characters
threadify :: String -> IO ()
threadify inputString = do
  let validWords = splitLongWords (words inputString) limit
      c          = map unwords $ chunks validWords [] [] 1 limit
      limit      = 280
  printThreads c

-- Each component of a thread has at most 280 characters
threadify' :: String -> IO ()
threadify' inputString = do
  let chunkGuess = estimateRequiredChunks inputString limit
      offset     = 2 * length (show chunkGuess) + (length separator)
      validWords = splitLongWords (words inputString) (limit - offset)
      limit      = 280
      startC     = Chunk [] 1 separator offset
      cll        = Node startC EmptyList
      filledCll  = insertList validWords limit cll
  putStrLn $ toString filledCll



printThreads :: [String] -> IO()
printThreads cs = do
  forM_ cs $ \c -> putStr $ c ++ "\n\n"

charCounts :: String -> [(Int, Int)]
charCounts str = scanl (\(index, acc) y -> (index + 1, acc + length y)) (0,0) (words str)

wordLengths :: String -> [(String, Int)]
wordLengths str = map (\s -> (s, length s)) (words str)

-- defines the separator between index and total
separator :: [Char]
separator = "/"

data ChunksArgs = ChunksArgs { availableWords :: [String]
                             , accumulator :: [[String]]
                             , currChunk :: [String]
                             , currIndex :: Int
                             , chunkLimit :: Int
                             , totalChunk :: Int
                             }

-- define Linked list for Chunks
-- define function to push word from first chunk to next one
-- define function to compute length of chunk
-- maybe linked list application
-- define function to push to chunk
data Chunk = Chunk { currentWords :: [String]
                   , numerator :: Int
                   , separatorExp :: String
                   , denominator :: Int
                   }
             deriving (Show, Read, Eq)

-- singlely linked chunk list
data ChunkLinkedList a = EmptyList | Node a (ChunkLinkedList a) deriving (Show, Read, Eq)

-- three coponents to the length of a Chunk
-- 1.) number of characters in words
-- 2.) number of characters used for spaces (number of words in currentWords)
-- 3.) number of characters used for the index
chunkLength :: Chunk -> Int
chunkLength c = length $ renderChunk c

-- render the chunk as a string
renderChunk :: Chunk -> String
renderChunk (Chunk cw n se d) = unwords cw ++ " " ++ show n ++ se ++ show d

insertAtBeginningChunk :: String -> [String] -> [String]
insertAtBeginningChunk w cw = w:cw

-- insert one word into a ChunkLinkedList
-- in order for this to work, we have to ensure the string is already broken up into a reasonable
-- size (i.e. below limit - index legnth).
insertOne :: String -> Int -> ChunkLinkedList Chunk -> ChunkLinkedList Chunk
insertOne w limit (Node c@(Chunk cw n _ _) EmptyList)
  | chunkLength newC <= limit = (Node newC EmptyList)
  | otherwise = (Node c (Node (c {currentWords = [w], numerator = n + 1}) EmptyList))
  where
    newCw = cw ++ [w]
    newC = c {currentWords = newCw}
insertOne w limit (Node c next) = Node c (insertOne w limit next)

-- insert a list of words into a ChunkLinkedList
insertList :: [String] -> Int -> ChunkLinkedList Chunk -> ChunkLinkedList Chunk
insertList [] _ cll = cll -- base case
insertList (w:ws) limit cll = insertList ws limit (insertOne w limit cll)

estimateRequiredChunks :: String -> Int -> Int
estimateRequiredChunks str limit = strChunks + indexChunks
  where
    strChunks = div (length str) limit + 1
    indexContribution = sum $ map length [show i ++ separator ++ show strChunks | i <- [1..strChunks]]
    indexChunks = div indexContribution limit + 1

-- get the number of chunks that are in the ChunkLinkedList
numChunks :: ChunkLinkedList a -> Int
numChunks EmptyList = 0
numChunks (Node _ next) = 1 + numChunks next

-- render the ChunkLinkedList as string with new lines between chunks
toString :: ChunkLinkedList Chunk -> String
toString EmptyList = ""
toString (Node c next) = renderChunk c ++ "\n\n" ++ toString next

chunks' :: ChunksArgs -> [[String]]
chunks' (ChunksArgs [] acc [] _ _ _) = acc
chunks' args@(ChunksArgs [] acc currentChunk index limit total)
  | getNewLen "" args <= limit = acc ++ [currentChunk ++ [show index ++ separator ++ show total]]
-- TODO need to do the case when > limit have to push words around between chunks
  | otherwise       = acc ++ [currentChunk ++ [show index ++ separator ++ show total]]
chunks' args@(ChunksArgs (x:xs) acc currentChunk index limit total)
  | getNewLen x args <= limit = chunks' (ChunksArgs xs acc (currentChunk ++ [x]) index limit total)
-- TODO need to do the case when > limit have to push words around between chunks
  | otherwise       = chunks' (ChunksArgs (x:xs) (acc ++ [currentChunk ++ [show index ++ separator ++ show total]]) [] (index+1) limit total)

getNewLen :: String -> ChunksArgs -> Int
getNewLen w args = length w + currentChunkLen + numberSpaces + indexLen
  where
    chunk = currChunk args
    currentChunkLen = sum $ map length chunk
    numberSpaces = length chunk - 1
    idx = currIndex args
    tot = totalChunk args
    -- length of index is sum of parts
    indexLen = sum $ map length [show idx, show tot, separator, " "]

-- input is list of words
-- output is list of string chunks
-- spaces count as characters so when checking for length we add in the
-- number of spaces that will be in the final chunk
chunks :: [String] -> [[String]] -> [String] -> Int -> Int -> [[String]]
chunks [] acc [] _ _ = acc
chunks [] acc currentChunk index _ = acc ++ [currentChunk ++ [show index ++ separator]]
chunks (x:xs) acc currentChunk index limit
  | newLen <= limit = chunks xs acc (currentChunk ++ [x]) index limit
  | otherwise = chunks (x:xs) (acc ++ [currentChunk ++ [show index ++ separator]]) [] (index+1) limit
  where
    currentChunkLen = sum $ map length currentChunk
    numberSpaces = length currentChunk - 1
    -- + 1 is for space needed to add index
    indexLen = (length $ show index) + length separator + 1
    newLen = length x + currentChunkLen + numberSpaces + indexLen

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
