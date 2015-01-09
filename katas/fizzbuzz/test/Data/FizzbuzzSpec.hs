module Data.FizzbuzzSpec where

import Test.Hspec
import Data.Fizzbuzz

main:: IO()
main = hspec spec

spec = do
  describe "fizzbuzz" $ do
    it "returns \"1\" for 1" $
      fizzbuzz 1 `shouldBe` Right 1

    it "returns \"2\" for 2" $
      fizzbuzz 2 `shouldBe` Right 2

    it "returns \"fizz\" for 3" $
      fizzbuzz 3 `shouldBe` Left "fizz"

    it "returns \"buzz\" for 5" $
      fizzbuzz 5 `shouldBe` Left "buzz"

    it "returs \"fizz\" for 9" $
      fizzbuzz 9 `shouldBe` Left "fizz"

    it "returs \"fizzbuzz\" for 15" $
      fizzbuzz 15 `shouldBe` Left "fizzbuzz"

    it "returns \"buzz\" for 25" $
      fizzbuzz 25 `shouldBe` Left "buzz"
