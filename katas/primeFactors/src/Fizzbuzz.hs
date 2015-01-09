module Fizzbuzz where

fizzbuzz :: Int -> Either String Int
fizzbuzz input = if null processedFizzbuzz
                    then Right input
                    else Left processedFizzbuzz
  where processedFizzbuzz = processFizzbuzz input

processFizzbuzz :: Int -> String
processFizzbuzz input = getFizz input ++ getBuzz input

getFizz :: Int -> String
getFizz input = validateDivizible (isDivizibleByThree input) "fizz"

getBuzz :: Int -> String
getBuzz input = validateDivizible (isDivizibleByFive input) "buzz"

validateDivizible :: Bool -> String -> String
validateDivizible isDivizible output = if isDivizible  then output else ""

isDivizibleByThree :: Int -> Bool
isDivizibleByThree = (`isDivizibleBy` 3)

isDivizibleByFive :: Int -> Bool
isDivizibleByFive = (`isDivizibleBy` 5)

isDivizibleBy :: Int -> Int -> Bool
input `isDivizibleBy` divider = 0 ==  input `mod` divider
