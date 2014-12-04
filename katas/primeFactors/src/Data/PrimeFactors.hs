module Data.PrimeFactors (primeFactors,isNumberDivizibleBy) where
primeFactors :: Int -> [Int]
primeFactors x = getFactorsListForNumber x 2 []

getFactorsListForNumber :: Int -> Int -> [Int] -> [Int]
getFactorsListForNumber number divider currentFactors
  | number < divider = currentFactors
  | otherwise = if isNumberDivizibleBy divider number
                then getFactorsListForNumber
                        (div number divider)
                        divider
                        (currentFactors ++ [divider])
                else
                  getFactorsListForNumber
                    number
                    (succ divider)
                    currentFactors

isNumberDivizibleBy :: Int -> Int -> Bool
isNumberDivizibleBy divider toDivide = toDivide `mod` divider == 0
