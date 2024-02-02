import Test.Hspec
import Lib (chunks, splitLongWords)
import Chunky

text1 :: String
text1 = "This is a test case to see if I can find one that messes up the estimated\
        \ chunkLength for different values of limit I guess"

main :: IO ()
main = hspec $ do
  describe "atool Lib" $ do
    it "Each word is less than limit" $ do
      let limit = 10
      let validWords = splitLongWords (words "aaaaaaaaaaa aaaaaaaaaaaaaa aa a aaaaaaaaaaaaaaaaaaaaa") limit
      all (<= limit) (map length validWords) `shouldBe` True
    it "Created chunks less than limit" $ do
      -- testing with input value of 10
      let limit = 20
      let c = chunks (words "This is only a test of the functionality of string tool") [] [] 1 limit
      all (<= limit) (map length $ map unwords c) `shouldBe` True

  -- TODO updated logic of fill
  describe "atool Chunk" $ do
    it "Created chunks are valid length" $ do
      let limit = 30
          cll = fill text1 limit
      all (<= limit) (chunkLengths cll) `shouldBe` True

    it "Number of chunks equals denominator after fill" $ do
      let limit              = 20
          cll@(Node c _)     = fill text1 limit
          nc                 = numChunks cll
      nc `shouldBe` denominator c

    it "toString Raw should be the same as original string" $ do
      let limit = 20
          cll   = fill text1 limit
          str   = toString Raw cll
      str `shouldBe` text1
