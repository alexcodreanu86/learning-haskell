factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * (factorial (x - 1))


-- Decomposing
head' :: [a] -> a
head' [] = error "Don't be stupid please!!!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstElement :: (Show a) => [a] -> String
firstElement all@(x:_) = "First element from " ++ (show all) ++ " is " ++ (show x)
firstTwoElements (x:y:_) = [x,y]

-- Guards
{-
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"

 -}
-- Or we can declare the same function refactored like this:
--
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat    = 30.0

        -- indentation in the where block has to be consistent
        -- we can also match in a where block like this:
        -- (skinny, normal, fat) = (18.5,25.0,30.0)
-- More on where blocks:
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- function declaration in where block
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis bmisList =  [bmi w h | (w,h) <- bmisList]
    where bmi weight height = weight / height ^ 2


-- let block declarations:
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * (r ^2)
  in sideArea + 2 * topArea

-- let block in funciton comprehension
calcBmisLet :: (RealFloat a) => [(a,a)] -> [a]
calcBmisLet bmisList =  [bmi | (weight,height) <- bmisList, let bmi = weight / height^2]

-- Function declaration with backticks
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT


fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci(x - 1) + fibonacci(x - 2)

firstFibs :: Int -> [Int]
firstFibs numOfFibs = [fibonacci currentNumber | currentNumber <- [1..numOfFibs]]

{-
maximum' :: (Ord a)  => [a] -> a
maximum' [] = error "Can't get maximum from an empty list!"
maximum' [x] = x
maximum' (x: xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

 -}
--OR:
maximum' :: (Ord a)  => [a] -> a
maximum' [] = error "Can't get maximum from an empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (firstElement:restOfElements) = firstElement : take' (n - 1) restOfElements


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:rest) =
  let smallerSorted = quickSort (filter (<= x) rest)
      biggerSorted = quickSort (filter (> x) rest)
  in smallerSorted ++ [x] ++ biggerSorted


-- Higher Order Functions

multThree ::(Num i) => i -> i -> i -> i
multThree x y z = x * y * z

multWithNine = multThree 9

multWithEighteen = multWithNine 2

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:xy) = f x y : zipWith' f xs xy

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x =  f x y

largestDivizibleBy :: (Integral a) => a -> a
largestDivizibleBy x = head (filter f [100000, 99999..])
  where f element = element `mod` x == 0

sumOfOddSquaresSmallerThan :: (Integral i) =>  i -> i
sumOfOddSquaresSmallerThan upperLimit =
  sum (takeWhile
        (< upperLimit)
        [square | n <- [1..],let square = n^2, odd square])

-- Lambdas             Collatz sequences
chain :: (Integral a)  => a -> [a]
chain 1 = [1]
chain n
  | even n = n: chain (n `div` 2)
  | otherwise = n: chain (n*3 + 1)

countChainsLongerThan :: Int -> Int
countChainsLongerThan x = length (filter (\xs -> length xs > x) (map chain [1..100]))

----------------- FOLDS -----------------
--sum rewrite
--foldl
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0 
{- foldl (fold left) just like reduce/inject in other languages starting from the left
 - foldl passes two arguments to the callback
    - first argument is the accumulator
    - second argument is the current element in the collection
 -}


--foldr
-- map rewrite
map' :: (a -> b) -> [a] -> [b]
map' f collection = foldr (\elem acc -> (f elem):acc) [] collection
{-
- foldr (fold right) traverses the container from the right
- foldr passes twi arguments to the callback
    - first argument is the current element in the collection
    - second argument is the accumulator
-}

-- foldl1 && foldr1
{-
- just like foldl and foldr but accumulator is the first element that gets processed by foldl or foldr
- sum' = foldl (+)
- they cause runtime errors if called with empty lists unlike foldl and foldr
-}

----------------- Scans -----------------
--scanl
{-
- skans are like folds only they report all the intermediate accumulator states in the form a list
-}

sqrtSums ::  Float -> Int
sqrtSums x = length (takeWhile (<x) (scanl1 (+) (map sqrt [1..]))) + 1

-- elem rewrite
elem' :: (Eq a) => a -> [a] -> Bool
elem' element container = foldl (\acc x -> if x == element then True else acc) False container


------------------ Function Application ($) ------------------
{-
- the $ function has low presedence
- it evaluates the expression on it's right and passes it to the expression on it's left
- sum (map sqrt [1..130]) can be rewritten like so :
- sum $ map sqrt[1..130]
-  or
-  sum (filter (> 10) (map (*2) [2..10])) as:
-  sum $ filter (> 10) $ map (*2) [2..10]
-}

------------------ Function Composition (.) ------------------
{-

f . g = \x -> f (g c)
- the argument received by f has to be the same type as the return on g function
map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] can be rewritten as:

map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
-}
