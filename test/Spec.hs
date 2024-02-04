import Test.Hspec
import Lib (chunks, splitLongWords)
import Chunky

text1 :: String
text1 = "This is a test case to see if I can find one that messes up the estimated\
        \ chunkLength for different values of limit I guess"

text2 :: String
text2 = "Scene I. On a ship at sea: a tempestuous noise of thunder and lightning heard.\
        \ Enter a Ship-Master and a Boatswain.\
        \ Mast. Boatswain!\
        \ Boats. Here, master: what cheer?\
        \ Mast. Good, speak to the mariners: fall to’t, yarely, or we run ourselves aground: \
        \ bestir, bestir. \
        \ Exit. \
        \ Enter Mariners. \
        \ Boats. Heigh, my hearts! cheerly, cheerly, my hearts! yare, yare! Take in the topsail. \
        \ Tend to the master’s whistle. Blow, till thou burst thy wind, if room enough! \
        \ Enter Alonso, Sebastian, Antonio, Ferdinand, Gonzalo, and others. \
        \ Alon. Good boatswain, have care. Where’s the master? Play the men. \
        \ Boats. I pray now, keep below. \
        \ Ant. Where is the master, boatswain? \
        \ Boats. Do you not hear him? You mar our labour: keep your cabins: you do assist the storm. \
        \ Gon. Nay, good, be patient. \
        \ Boats. When the sea is. Hence! What cares these roarers for the name of king? \
        \ To cabin: silence! trouble us not. \
        \ Gon. Good, yet remember whom thou hast aboard. \
        \ Boats. None that I more love than myself. You are a 20 Counsellor; \
        \ if you can command these elements to silence, and work the peace of the present, \
        \ we will not hand a rope more; use your authority: if you cannot, give thanks \
        \ you have lived so long, and make yourself ready in your cabin for the mischance \
        \ of the hour, if it so hap. Cheerly, good I. 1. 25 hearts! Out of our way, I say. \
        \ Exit. \
        \ Gon. I have great comfort from this fellow: \
        \ methinks he hath no drowning mark upon him; \
        \ his complexion is perfect gallows. Stand fast, good Fate, to his hanging:\
        \ make the rope of his destiny our cable, for our own doth 30 little advantage.\
        \ If he be not born to be hanged, our case is miserable."

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
      let limit = 60
          cll   = fill text2 limit
          nc    = numChunks cll
      nc `shouldBe` getDenominator cll

    it "toString Raw should be the same as original string" $ do
      let limit = 20
          cll   = fill text1 limit
          str   = toString Raw cll
      str `shouldBe` text1
