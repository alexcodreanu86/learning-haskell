module PrimeFactorsSpec where

import Test.Hspec
import PrimeFactors

main :: IO ()
main = hspec $ do
  describe "primeFactors" $ do
    it "returns an [2] for 2" $ do
      primeFactors 2 `shouldBe` [2]

    it "returns [3] for 3" $ do
      primeFactors 3 `shouldBe` [3]

    it "returns [2,2] for 4" $ do
      primeFactors 4 `shouldBe` [2,2]

  describe "isNumberDivizibleBy" $ do
    it "returns false when numbers are NOT divizible" $ do
      isNumberDivizibleBy 10 
