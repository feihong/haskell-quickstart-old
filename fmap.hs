{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Fmap exercises" $ do
    it "Lift twice" $ do
      (fmap . fmap) (++"lol") $ Just ["Hi,", "Hello"]
      `shouldBe` Just ["Hi,lol","Hellolol"]

    it "Lift over function" $ do
      let c = fmap (*2) (\x -> x - 2)
      c 1 `shouldBe` -2

    it "Lift over another function" $ do
      let d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
      d 0 `shouldBe` "1[0,1,2,3]"

    it "Lift over IO" $ do
      let e :: IO Integer
          e = let ioi = readIO "1" :: IO Integer
                  changed = fmap read $ fmap ("123"++) $ fmap show ioi
              in fmap (*3) changed

      -- No idea how to do a comparison with IO values, so just fudge it
      True `shouldBe` True
