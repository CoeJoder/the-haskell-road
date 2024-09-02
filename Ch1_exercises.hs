import GS

-- Exercise 1.9
mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

-- Exercise 1.10 - define a function removeFst that removes the first occurrence
--   of an integer m from a list of integers.  If `m` does not occur in the list,
--   the list remains unchanged.
-- removeFst :: Int -> [Int] -> [Int]
-- removeFst m [] = []
-- removeFst m [x] | m == x = []
--                 | m /= x = [x]
-- removeFst m (xy:x:xs) | m == x = xy:xs
--                       | m /= x = xy:x:xs
-- removeFst m (x:xs) | m == x = xs
--                    | m /= x = x:xs
removeFst :: Eq a => a -> [a] -> [a]
removeFst x [] = []
removeFst x (y:ys) | x == y = ys
                   | otherwise = y : (removeFst x ys)

-- This time, remove all occurrencess not just the first
removeAll :: Eq a => a -> [a] -> [a]
removeAll x [] = []
removeAll x (y:ys) | x == y = removeAll x ys
                   | otherwise = y : removeAll x ys

-- Example 1.11 - define a function that sorts a list of integers in order of increasing size
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : srtInts (removeFst m xs) where m = mnmInt xs

-- This time, using `let` instead
srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let
                m = mnmInt xs
              in
                m : srtInts' (removeFst m xs)

-- Exercise 1.13 - write a function `count` for counting the number of occurrences of a character in a string.
-- In Haskell, a character is an object of type `Char`, and a string an object of type `String`, so the type
-- declaration should run: `count :: Char -> String -> Int`
count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x    = 1 + count c xs
               | otherwise = count c xs

-- Exercise 1.14 - a function for transforming strings into strings is of type `String -> String`.
-- Write a function `blowup` that converts a string `a1,a2,a3` to `a1,a2,a2,a3,a3,a3`.
-- E.g. `blowup "bang!"` should yield "baannngggg!!!!!" (hint: use ++ for string concatenation)
copy :: Int -> Char -> String
copy 0 c = []
copy n c = c : copy (n-1) c

blowup :: String -> String
blowup xs = blowup' 1 xs

blowup' :: Int -> String -> String
blowup' _ [] = []
blowup' n (x:xs) = copy n x ++ blowup' (n+1) xs

-- Exercise 1.15 - write a function srtString :: [String] -> [String] that sorts a list of strings
-- in alphabetical order.
mnmOrd :: Ord a => [a] -> a
mnmOrd [] = error "empty list"
mnmOrd [x] = x
mnmOrd (x:xs) = min x (mnmOrd xs)

srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : srtString (removeFst m xs) where m = mnmOrd xs

-- Exercise 1.17 - Write a function substring :: String -> String -> Bool that checks whether str1 is a substring of str2.
-- The substrings of an arbitrary string ys are given by:
-- if xs is a prefix of ys, xs is a substring of ys
-- if ys equals y:ys' and xs is a substring of ys', xs is a substring of ys
-- nothing else is a substring of ys
--E.g.
--  xs  = "sauce"
--  ys  = "sauces"
--    y   = "s"
--    ys' = "auces"
substring :: String -> String -> Bool
substring _ [] = False
substring xs ys | prefix xs ys = True
substring xs (y:ys) = substring xs ys
-- NOTE: this differs signficantly from the solutions guide, but seems to work fine...

-- **Exercise 1.18** - Find expressions with the following types:
-- 1. `[String]`          : ["hello", "world!"]
-- 2. `(Bool, String)`    : (True, "foobar")
-- 3. `[(Bool, String)]`  : [(True, "foo"), (False, "bar")]
-- 4. `([Bool], String)`  : ([False, True, False], "foobaz")
-- 5. `Bool -> Bool`      : \x -> not (not x)
-- Test your answers by means of the interpreter command `:t`

-- **Exercise 1.19** - Use the interpreter command `:t` to find the types of the following predefined functions:
-- 1. `head`      :: GHC.Stack.Types.HasCallStack => [a] -> a
--   head ["1", "2", "3"] = "1" -- gets first element from a list/data structure
-- 2. `last`      :: GHC.Stack.Types.HasCallStack => [a] -> a
--   last ["1", "2", "3"] = "3" -- gets last element from a list/data structure
-- 3. `init`      :: GHC.Stack.Types.HasCallStack => [a] -> [a]
--   init ["1", "2", "3"] = ["1", "2"] -- gets all but the last element from a list/data structure
-- 4. `fst`       :: (a, b) -> a
--   fst (1, "2") = 1 -- gets the first element of a pair (doesn't work with tuples, only pairs)
-- 5. `(++)`      :: [a] -> [a] -> [a]
--   [1, 2] ++ [3, 4, 5] = [1,2,3,4,5] -- combines two lists
-- 6. `flip`      :: (a -> b -> c) -> b -> a -> c
--   flip (\x -> (\y -> x ++ y)) "foo" "bar" = "barfoo"
-- or with syntax sugar:
--   flip (\x y -> x ++ y) "foo" "bar" = "barfoo" -- calls the function passed as the first argument with the subsequent arguments, but reversed
-- 7. `flip (++)` :: [a] -> [a] -> [a]
--   flip (++) "foo" "bar" = "barfoo" -- same as above, but using ++ operator is converted to prefix notation and passed as the argument, and all operands,
--     i.e. the arg to the first function, the arg to the second function, and the return value, are all of the same type `[a]`
-- Next, supply these functions with arguments of the expected types, and try to guess what these functions do.

-- **Exercise 1.20** - Use `map` to write a function `lengths` that takes a list of lists and returns a list of the corresponding list lengths.
lengths :: [[a]] -> [Int]
lengths = map length

-- **Exercise 1.21** - Use `map` to write a function of `sumLengths` that takes a list of lists and returns the sum of their lengths.
sumLengths :: [[a]] -> Int
sumLengths = foldr ((+) . length) 0
-- or, actually using `map`:
sumLengths' :: [[a]] -> Int
sumLengths' xs = sum (map length xs)
-- or, using function composition:
sumLengths'' :: [[a]] -> Int
sumLengths'' = sum . lengths
