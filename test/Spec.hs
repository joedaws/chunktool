import Test.Hspec
import Lib (chunks, splitLongWords)
import GHC.Real (FractionalExponentBase(Base10))

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
