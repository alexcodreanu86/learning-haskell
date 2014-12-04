module Data.Fizzbuzz where

fizzbuzz :: Int -> String
fizzbuzz x = let response = processFizzbuzz x
             in if empty response then show x else response

processFizzbuzz :: Int -> String
processFizzbuzz number
  = getWordIfValid (isBuzz number) "buzz" $
      getWordIfValid (isFizz number) "fizz" cummulator
    where cummulator = ""

getWordIfValid :: Bool -> String -> String -> String
getWordIfValid validator expectedResponse cummulator
  | validator = cummulator ++ expectedResponse
  | otherwise = cummulator

empty :: [a] -> Bool
empty = null

isFizz :: Int -> Bool
isFizz = (`isDivisibleBy` 3)

isBuzz :: Int -> Bool
isBuzz = (`isDivisibleBy` 5)

isDivisibleBy :: Int -> Int -> Bool
number `isDivisibleBy` divider =  number `mod` divider == 0
