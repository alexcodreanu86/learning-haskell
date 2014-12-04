module Data.FizzbuzzSpec where

import Test.Hspec
import Data.Fizzbuzz

main:: IO()
main = hspec spec

spec = do
  describe "fizzbuzz" $ do
    it "returns \"1\" for 1" $
      fizzbuzz 1 `shouldBe` "1"

    it "returns \"2\" for 2" $
      fizzbuzz 2 `shouldBe` "2"

    it "returns \"fizz\" for 3" $
      fizzbuzz 3 `shouldBe` "fizz"

    it "returns \"buzz\" for 5" $
      fizzbuzz 5 `shouldBe` "buzz"

    it "returs \"fizz\" for 9" $
      fizzbuzz 9 `shouldBe` "fizz"

    it "returs \"fizzbuzz\" for 15" $
      fizzbuzz 15 `shouldBe` "fizzbuzz"

    it "returns \"buzz\" for 25" $
      fizzbuzz 25 `shouldBe` "buzz"
