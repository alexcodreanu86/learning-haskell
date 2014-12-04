module PrimeFactorsSpec where

import Test.Hspec
import PrimeFactors

main :: IO ()

main = hspec $ do
  describe "primeFactors" $ do
    it "returns [] for any number smaller than 2" $
      primeFactors 1 `shouldBe` []

    it "returns [2] for 2" $
      primeFactors 2 `shouldBe` [2]

    it "returns [3] for 3" $
      primeFactors 3 `shouldBe` [3]

    it "returns [2,2] for 4" $
      primeFactors 4 `shouldBe` [2,2]

    it "returns [3,3] for 4" $
      primeFactors 9 `shouldBe` [3,3]

    it "returns [2,2,3] for 12" $
      primeFactors 12 `shouldBe` [2,2,3]

    it "returns [2,3,3,3,5] for 270" $
      primeFactors 270 `shouldBe` [2,3,3,3,5]

  describe "isNumberDivizibleBy" $ do
    it "returns false when numbers are NOT divizible" $
      isNumberDivizibleBy 3 10 `shouldBe` False

    it "returns true when numbers are divizible" $
      isNumberDivizibleBy 5 10 `shouldBe` True
