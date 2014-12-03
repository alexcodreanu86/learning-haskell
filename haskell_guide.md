###Functions
Declaring a function:
```haskell
myAdd param1 param2 = param1 + param2
```

Invoking the function:
```haskell
myAdd 2 3
-- => 5
```

Also if a function takes two parameters it can be called as an infix function (between the two parameters) by surrounding it with backticks '`'
```haskell
2 `myAdd` 3
-- => 5

--which is the same as
2 + 3
-- => 5
```

Functions can be declared with back ticks for better readability
```haskell
a `myAdd` b = a + b

1 `myAdd` 2
-- => 3

myAdd 1 2
-- => 3
```

###Lists
Declaring a list:
```haskell
myList = [1,3,4]
```

Joining two lists:
```haskell
[1,2,3] ++ [4,5]
-- => [1,2,3,4,5]
```

Appending the front of a list:
```haskell
4:[1,2,3]
-- => [4,1,2,3]
```

Accessing element by index:
```haskell
[1,2,3] !! 1
-- => 2
```

Accessing the first element:
```haskell
head [1,2,3]
-- => 1
```

Accessing the last element:
```haskell
last [1,2,3]
-- => 3
```

Getting All elements except the first one:
```haskell
tail [1,2,3]
-- => [2,3]
```

Getting All elements except the last one:
```haskell
init [1,2,3]
-- => [1,2]
```

Getting list length:
```haskell
length [1,2,3]
-- => 3
```

Checking if a list is empty:
```haskell
null [1,2,3]
-- =>  false

null []
-- => true
```

Take a given number of elements from the begining of the list
```haskell
take 2 [1,2,3]
-- =>  [1,2]
```

Drop a given number of elements from a list and returns the remainder:
```haskell
drop 2 [1,2,3]
-- => [3]
```

Return the bigest element in a list:
```haskell
maximum [1,2,4,3]
-- =>  4
```

Sum of all elements in a list:
```haskell
sum [1,2,3,4]
-- => 10
```

Product of all elements in a list:
```haskell
product [1,2,3,4]
-- => 24
```

Check if a list contains an element:
```haskell
elem 3 [1,2,3]
-- => true

--infix version:
3 `elem` [1,2,3]
-- => true

elem 4 [1,2,3]
-- => false
```

Note Strings in Haskell are just lists of characters:
```haskell
'A':['l','e','x']
-- => "Alex"

'A':"lex"
-- => "Alex"
```

###Ranges
Integer and Character ranges can be generated easily:
```haskell
[1..10]
-- => [1,2,3,4,5,6,7,8,9,10]

['a'..'f']
-- => "abcdef"

['K'..'Q']
-- => "KLMNOPQ"
```

Ranges can also specify a step:
```haskell
-- odd numbers up to 20
[2,4..20]
-- => [2,4,6,8,10,12,14,16,18,20]

-- other step
[5,10..47]
-- => [5,10,15,20,25,30,35,40,45]

-- works for characters too
['a','c'..'q']
-- => "acegikmoq"

-- watch out for floating ranges because they are not very accurate
[0.1,0.3..1]
-- => [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]
```

You can cycle elements in a list:
```haskel
take 5 (cycle[1,2,3])
-- => [1,2,3,1,2]
{-
calling just cycle on the list will create an infinite loop cycling through the list.
make sure to slice it up somewhere
-}
```

You can also `repeat` an element:
```haskell
take 6 (repeat 5)
-- => [6,6,6,6,6,6]

{-
just like cycle, repeat generates an infinite list of the given element
-}
```

You can `replicate` an element a given number of times in a list:
```haskell
replicate 3 5
-- => [5,5,5]
```


###Sequence comprehension

You can collect the processed value of each element of a list like this:
```haskell
[x * 2| x <- [1..5]]
-- => [2,4,6,8,10]
```

You can select just the desired values from a  list:
```haskell
[x | x <- [1..10], odd x]
-- => [1,3,5,7,9]

-- you can have many filtering conditions
[x | x <- [1..20], odd x, (x `mod` 3) == 0]
-- => [3,9,15]
```

Or you can combine filtering and value processing:
```haskell
[x * 2 | x <- [1..10], odd x]
-- => [2,6,10,14,18]
```

You can draw form multiple lists:
```haskell
[x * y| x <- [1,3,5], y <- [2,4,6]]
-- => [2,4,6,6,12,18,10,20,30]

-- with filtering:
[x * y| x <- [1,3,5], y <- [2,4,6], x * y > 10]
-- => [12,18,20,30]
```

Comprehension on nested lists is possible too:
```haskell
let nestedLists = [[1,2,3,4],[5,6,7,8],[26,27]]
[ [x | x <- xs, even x]
     | xs <- nestedLists]
-- => [[2,4],[6,8],[26]]  all the even numbers in each nested list
```

###Tuples
In some ways, tuples are like lists — they are a way to store several values into a single value. However, there are a few fundamental differences. A list of numbers is a list of numbers. That's its type and it doesn't matter if it has only one number in it or an infinite amount of numbers. Tuples, however, are used when you know exactly how many values you want to combine and its type depends on how many components it has and the types of the components. They are denoted with parentheses and their components are separated by commas.

Another key difference is that they do NOT have to be homogenous. Unlike a list, a tuple can contain a combination of several types.
Defining a list of tuples:
```
-- valid:
[(1,"One"),(2,"two"),(3,"three")]

-- invalid:

[(1,"One"),(2,"two","third element"),(3,"three")]
```

A 2 element tuple is called a pair and they come with the next helper methods:
```haskel
fst (1, "one")
-- => 1

snd (1, "one")
-- => "one"
```

We can group pairs from two different list:
```haskel
zip [1,2,3,4] ["one","two","three","four"]
-- => [(1,"one"),(2,"two"),(3,"three"),(4,"four")]

-- watch what happens when one of the list is shorter than the other one:

zip [1,2,3,4,5] ["one","two","three","four"]
-- => [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
-- the longer list gets cut off to match the shorter list

zip [1..] ["one","two","three","four"]
-- => [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
-- a lazy sequence building can be used here
```

###Types
Methods Declaration can include variable types and returntype:
```haskell
doubleMe:: Int -> Int->
doubleMe x = x * 2

-- The last Type declared is the type of the value returned
```

*Int* | integer type with max value of 2147483647 and minimum value of -2147483648
*Integer* | unbounded integer(for smaller values it is more efficient to use Int type)
*Float* | real floating point with single precision
*Double* | real floating point with double precision
*Bool* | Boolean type can be `True` or `False`
*Char* | Character denoted by single quotes

### Methods
`show` equivalent to `toString`
`read` evaluates the value in the string returning the type that is required by the function that uses the returned value

###Decomposing arguments

List arguments can be decomposed
```haskell
firstElement (x:_) = x

firstElement [1,2,3]
-- => 1

firstTwoElements (x:y:_) = [x,y]
```

Also one can access the entire parameter while decomposing it:
```haskell
firstElement all@(x:_) = "First element from " ++ (show all) ++ " is " ++ (show x)
```

### Where and Let bindings
- `where` bindings bind variable names with values or functions that have scope throughout the function including guards
```haskell
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials "Alex" "Codreanu"
-- =>  "A. C."
```

- `let` bindings bind variable names with values or functions with scope that does NOT span across guards
- the bindings inside `let` only have scope inside the `in` block
- `let` bindings are expressions that can be used anywhere while `where` bindings are just syntactic constructs

the form is `let <binding> in <expression>`
```haskell
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * (r ^2)
  in sideArea + 2 * topArea

cylinder 1 2
-- => 18.84955592153876
4 * (let square x = x * x  in square 5) - 3
-- =>  97
-- this could be done easier by just calculating the square of 5 but it proves the point that it can be done
```

To bind several variables inline inside a let statement statements can be separated by semicolons `;`
```haskell
(let x = 1; y = 3; z = 4; square a = a * a in (square x, square y, square z))
-- =>  (1,9,16)
```

`let` statements can be included inside list comprehension
```haskell
calcBmis :: (RealFloat f) => [(a,a)] -> [a]
calcBmis bmisList = [bmi | (weight,height) <- bmisList, let bmi = weight / height ^ 2]
calcBmis
```

### Case Expressions

In Haskell patern matching for arguments is actually syntactic sugar for creating a case expression. Here are two method definitions that do the exact same thing:

```haskell
-- Partern matching version
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x

-- Case expression version:
head' xs =  case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x
```

Syntax for case expressions is:
```Haskell
case expression of pattern -> result
                   pattern -> result
                   pattern -> result
```

If no pattern match is found a run time error is thrown
Whereas pattern matching on function parameters can only be done when defining functions, case expressions can be used pretty much anywhere. For instance:
```Haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
```

They are useful for pattern matching against something in the middle of an expression. Because pattern matching in function definitions is syntactic sugar for case expressions, we could have also defined this like so:
```Haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
```
