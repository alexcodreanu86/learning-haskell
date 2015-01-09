module Data.Fizzbuzz where

fizzbuzz :: Int -> Either String Int
fizzbuzz x
  | null response = Right x
  | otherwise = Left response
  where response = processFizzbuzz x

processFizzbuzz :: Int -> String
processFizzbuzz number
  = getWordIfValid (isBuzz number) "buzz" $
      getWordIfValid (isFizz number) "fizz" ""

getWordIfValid :: Bool -> String -> String -> String
getWordIfValid validator expectedResponse cummulator
  | validator = cummulator ++ expectedResponse
  | otherwise = cummulator

isFizz :: Int -> Bool
isFizz = (`isDivisibleBy` 3)

isBuzz :: Int -> Bool
isBuzz = (`isDivisibleBy` 5)

isDivisibleBy :: Int -> Int -> Bool
number `isDivisibleBy` divider =  number `mod` divider == 0
