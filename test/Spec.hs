import Test.Hspec
import Lib (chunks)

main :: IO ()
main = hspec $ do
  describe "atool Lib" $ do
    it "all chunks less than limit" $ do
      -- testing with input value of 10
      let limit = 20
      let c = chunks (words "This is only a test of the functionality of string tool") [] [] 1 limit
      all (<= limit) (map length $ map unwords c) `shouldBe` True
