module Chunky (
     Chunk(..)
    , chunkify
    , ChunkLinkedList(..)
    , chunkLength
    , chunkLengths
    , estimateRequiredChunks
    , fill
    , getDenominator
    , numChunks
    , renderChunk
    , RenderMode(..)
    , setDenominatorForAll
    , toString
    ) where

chunkify :: String -> IO ()
chunkify inputString = do
  -- TODO handle the case when words are too long
  let limit        = 280
      c            = fill inputString limit
      outputString = toString Indexed c
  putStrLn outputString

-- defines the separator between index and total
separator :: [Char]
separator = "/"

-- Linked list for Chunks
-- define function to push word from first chunk to next one
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
chunkLength c = length $ renderChunk Indexed c

chunkLengths :: ChunkLinkedList Chunk -> [Int]
chunkLengths (Node c EmptyList) = (chunkLength c):[]
chunkLengths (Node c next) = (chunkLength c):(chunkLengths next)

data RenderMode = Raw | Indexed

-- render the chunk as a string
renderChunk :: RenderMode -> Chunk -> String
renderChunk Indexed (Chunk cw n se d) = unwords cw ++ " " ++ show n ++ se ++ show d
renderChunk Raw c = unwords $ currentWords c

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
estimateRequiredChunks str limit
  | length str + length "1/1" <= limit = 1
  | otherwise                          = strChunks + indexChunks
  where
    strChunks = div (length str) limit + 1
    indexContribution = sum $ map length [show i ++ separator ++ show strChunks | i <- [1..strChunks]]
    indexChunks = div indexContribution limit + 1

-- get the number of chunks that are in the ChunkLinkedList
numChunks :: ChunkLinkedList a -> Int
numChunks EmptyList = 0
numChunks (Node _ next) = 1 + numChunks next

-- render the ChunkLinkedList as string with new lines between chunks and with chunk index
toIndexedString :: ChunkLinkedList Chunk -> String
toIndexedString EmptyList = ""
toIndexedString (Node c next) = renderChunk Indexed c ++ "\n\n" ++ toIndexedString next

-- render the ChunkLinkedList as a string with no new lines between chunks
toRawString :: ChunkLinkedList Chunk -> String
toRawString EmptyList = ""
toRawString (Node c EmptyList) = renderChunk Raw c
toRawString (Node c next) = renderChunk Raw c ++ " " ++ toRawString next

toString :: RenderMode -> ChunkLinkedList Chunk -> String
toString Raw c = toRawString c
toString Indexed c = toIndexedString c

-- a Chunk to start with
initialChunk :: String -> Int -> Chunk
initialChunk inputString limit = Chunk [] 1 separator chunkGuess
  where
    chunkGuess = estimateRequiredChunks inputString limit

-- initialize the ChunkLinkedList from a string
-- assume all words are less than limit
fill :: String -> Int -> ChunkLinkedList Chunk
fill s limit = fixDenominator filled nc
  where
    initC   = initialChunk s limit
    initCll = Node initC EmptyList
    filled  = insertList (words s) limit initCll
    nc      = numChunks filled

setDenominatorForAll :: ChunkLinkedList Chunk -> Int -> ChunkLinkedList Chunk
setDenominatorForAll (Node c EmptyList) newDenominator = Node c {denominator = newDenominator} EmptyList
setDenominatorForAll (Node c next) newDenominator = Node c {denominator = newDenominator} (setDenominatorForAll next newDenominator)

getDenominator :: ChunkLinkedList Chunk -> Int
getDenominator EmptyList = 0
getDenominator (Node c EmptyList) = denominator c
getDenominator (Node c next) = denominator c

-- For a ChunkList we set the new denominator
-- when the new denominator has string length
-- less than or equal the old one, then we simply
-- update the value (no need to shuffle words).
-- When the new denominator length is more than
-- the previous one, we refill the chunk list
-- with the new denominator and check so see
-- if the list is still correct.
fixDenominator :: ChunkLinkedList Chunk -> Int -> ChunkLinkedList Chunk
fixDenominator cll newDenom
  | oldDenom == newDenom = cll -- do nothing in this case
  | oldDenom /= newDenom = if newDenomLength <= oldDenomLength
                           then setDenominatorForAll cll newDenom
                           else fixDenominator newCll newNewDenom
  where
    newDenomLength = length $ show newDenom
    oldDenom       = getDenominator cll
    oldDenomLength = length $ show oldDenom
    newCll         = fill (toString Raw cll) newDenom
    newNewDenom    = getDenominator newCll
