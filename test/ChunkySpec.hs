module ChunkySpec (spec) where

import Test.Hspec
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

text3 :: String
text3 = "Time is at a premium and condensing or filtering information seem to be natural approaches to consume more. " ++
        "In our current environment this approach may be attractive (or at least I feel a pull in this direction " ++
        "sometimes) so that we feel we are maintaining a productive stance despite the ever increasing deluge of " ++
        "information. It's natural I think that I turn to technology to help me out. In particular, LLMs seem well " ++
        "suited to performing summarization of text. However, just because a well formed summary can be created for " ++
        "any information these days, we need to be judicious about where we expect summaries to provide value to " ++
        "people and businesses.\n\n" ++
        "Suppose that I wanted to read a James Joyce novel. I might be intimidated by the length or density of the " ++
        "work (I'm a bit ashamed but this is true). A dark pattern would be to simply consume the summary. Facts " ++
        "have been conveyed and consumed by me having read the summary. However, the value of the work doesn't lie " ++
        "in the facts but in the process of consuming the work in the first place. Then I'm compelled to read the " ++
        "work directly. Upon starting I may find the novel totally inaccessible to me, \"I can't follow the narrative " ++
        "structure, I don't understand these references!\" I think a good path forward is to use the LLM in conjunction " ++
        "with the original material. Similar to how I use a dictionary app when reading on e-ink devices to define " ++
        "words that are unfamiliar to me. A similar strategy may work for dense works of literature (though there is " ++
        "always a danger of getting derailed while reading the meaning of each aside).\n\n" ++
        "There are surely other instances of use-cases of LLMs where the technology can be used to enhance the " ++
        "consumption of existing text while not totally removing the necessity and complexity of the source text."

spec :: Spec
spec = describe "chunktool Chunk" $ do
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

    it "Complex example test fill" $ do
      let limit  = 300
          cll    = fill text3 limit
          chunks = toList cll
      all (<= limit) (map length $ map (renderChunk Indexed) chunks) `shouldBe` True
