module BowlingSpec where

import Bowling
import Test.Hspec

generateScores :: Int -> Int -> [Int]
generateScores value times = replicate times value

main :: IO()
main = hspec $ do
  describe "getScores" $ do
    it "returns 0 for []" $
      getScore [] `shouldBe` 0

    it "returns 5 for [5]" $ do
      getScore (10:(generateScores 0 18)) `shouldBe` 10

    it "returns 13 for [ 5, 2, 5, 1 ]" $ do
      getScore ([ 5, 2, 5, 1 ] ++ replicate 16 0)  `shouldBe` 13

    context "in case of spare" $ do
      it "returns 17 for [ 5, 5, 2, 3 ]" $
        getScore ([ 5, 5, 2, 3 ] ++ replicate 16 0) `shouldBe` 17

      it "returns 150 for gutter game" $
        getScore (replicate 21 5) `shouldBe` 150

    context "in case of strike" $ do
      it "returns 20 for [10, 2, 3]" $
        getScore ([ 10, 2, 3 ] ++ replicate 16 0) `shouldBe` 20

      it "returns 300 for all strikes" $
        getScore(replicate 12 10) `shouldBe` 300
