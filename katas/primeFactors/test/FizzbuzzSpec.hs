{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module FizzbuzzSpec (main, spec) where

import Test.Hspec
import Fizzbuzz

main:: IO()
main = hspec spec

spec = do
  describe "fizzbuzz" $ do
    it "returns 1 for 1" $
      fizzbuzz 1 `shouldBe` Right 1

    it "returns 2 for 2" $
      fizzbuzz 2 `shouldBe` Right 2

    it "returns \"fizz\" for 3" $
      fizzbuzz 3 `shouldBe` Left "fizz"

    it "returns \"buzz\" for 5" $
      fizzbuzz 5 `shouldBe` Left "buzz"

    it "returns \"fizz\" for 6" $
      fizzbuzz 6 `shouldBe` Left "fizz"

    it "returns \"buzz\" for 10" $
      fizzbuzz 10 `shouldBe` Left "buzz"

    it "returns \"fizzbuzz\" for 15" $
      fizzbuzz 15 `shouldBe` Left "fizzbuzz"

  describe "isDivizibleBy" $ do
    it "returns True for 6 and 2" $
      6 `isDivizibleBy` 2 `shouldBe` True

  describe "isDivizibleByThree" $ do
    it "returns True for 6" $
      isDivizibleByThree 6 `shouldBe` True

    it "returns False for 5" $
      isDivizibleByThree 5 `shouldBe` False

  describe "isDivizibleByFive" $ do
    it "returns False for 6" $
      isDivizibleByFive 6 `shouldBe` False

    it "returns True for 5" $
      isDivizibleByFive 5 `shouldBe` True
