-- Tyler Schulenberg
-- CMPS 112
-- Haskell HW # 2

-- 1
-- Using recursion, write a function
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- which behaves just like the standard foldl.


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl function acc [] = acc
myFoldl function acc (head:tail) = myFoldl function (function acc head) tail

-- 2
-- Using the standard foldl (not myFoldl), write a function
-- myReverse :: [a] -> [a]
-- which behaves just like the standard reverse.

myReverse :: [a] -> [a]
myReverse input = foldl (flip(:)) [] input

-- 3
-- Using the standard foldl (not myFoldl), write a function
-- myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- which behaves just like the standard foldr.

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr function acc bs = foldl (\g b x -> g (function b x)) id bs acc

-- 4
-- Using the standard foldr (not myFoldr), write a function
-- myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
-- which behaves just like the standard foldl.

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 function acc bs = foldr (\b g x -> g (function x b)) id bs acc

-- 5
-- Write a function
-- isUpper :: Char -> Bool
-- which returns true if the provided character is in the range 'A' to 'Z'.

isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

-- 6
-- Using the standard filter, write a function
-- onlyCapitals1 :: String -> String
-- which returns only the capital letters of the provided string
-- > onlyCapitals1 "Hello, World!"
-- "HW"

onlyCapitals1 :: String -> String
onlyCapitals1 [] = ""
onlyCapitals1 (head:tail) = if isUpper head then head : onlyCapitals1 tail else onlyCapitals1 tail

-- 7
-- Using list comprehension, write a function
-- onlyCapitals2 :: String -> String
-- which returns only the capital letters of the provided string.
-- Hint: Use the isUpper function from question 5


onlyCapitals2 :: String -> String
onlyCapitals2 x = [ c | c <- x, isUpper c]

-- 8
-- Using recursion, write a function
-- onlyCapitals3 :: String -> String
-- which returns only the capital letters of the provided string.

onlyCapitals3 :: String -> String
onlyCapitals3 [] = ""
onlyCapitals3 (x:xs)
	| (isUpper x == True) = x:onlyCapitals3 xs
	| (isUpper x == False) = onlyCapitals3 xs

-- 9
-- Write a function
-- divRemainder :: Int -> Int -> (Int, Int)
-- which returns a tuple with the quotient and the remainder of an integer division of the provided two numbers:
-- > divRemainder 12 4
-- (3,0)
-- > divRemainder 23 5
-- (4,3)

divRemainder :: Int -> Int -> (Int, Int)
divRemainder x y = ((div x y),(mod x y))

-- 10
-- Write a function
-- digitSum :: Int -> Int
-- which returns the sum of the digits of the given integer:

digitSum :: Int -> Int
digitSum x
	| x == 0 = 0
	| otherwise = (mod x 10) + (digitSum (div x 10))

-- 11
-- sayNum x
-- which takes a string of digits and spells out the number as string in English.
-- The number will be between 1 and 10^66 - 1, so you have to support names for big numbers from thousand and million up to vigintillion.

sayNum :: String -> String
sayNum x = helpMe 0 (read x)

helpMe :: Integer -> Integer -> String
helpMe depth x
	| x == 0 = ""
	| ((fromIntegral(length(show x))) == 1) = ones(mod x 10)
	| ((fromIntegral(length(show x))) == 2) = tens(mod(div x 10)10) ++ ones(mod x 10)
	| otherwise = helpMe (incremenate_depth) (div x 1000) ++  (ones(mod(div x 100)10) ++ "hundred " ++ tens(mod (div x 10) 10) ++ ones(mod x 10)) ++ largeNum depth
	where incremenate_depth = depth + 1
ones :: Integer -> String
ones x
	| (x == 0) = ""
	| (x == 1) = "one "
	| (x == 2) = "two "
	| (x == 3) = "three "
	| (x == 4) = "four "
	| (x == 5) = "five "
	| (x == 6) = "six "
	| (x == 7) = "seven "
	| (x == 8) = "eight "
	| (x == 9) = "nine "
	| otherwise = "fail "
	
tens :: Integer -> String
tens x
	| (x == 0) = ""
	| (x == 1) = "ten "
	| (x == 2) = "twenty "
	| (x == 3) = "thirty "
	| (x == 4) = "forty "
	| (x == 5) = "fifty "
	| (x == 6) = "sixty "
	| (x == 7) = "seventy "
	| (x == 8) = "eighty "
	| (x == 9) = "ninety "
	| otherwise = "fail "

largeNum :: Integer -> String
largeNum x
	| (x == 1) = "thousand "
	| (x == 2) = "million "
	| (x == 3) = "billion "
	| (x == 4) = "trillion "
	| (x == 5) = "quadrillion "
	| (x == 6) = "quintillion "
	| (x == 7) = "sextillion "
	| (x == 8) = "septillion "
	| (x == 9) = "octillion "
	| (x == 10) = "nonillion "
	| (x == 11) = "deillion "
	| (x == 12) = "undecillion "
	| (x == 13) = "duodecillion "
	| (x == 14) = "tredecillion "
	| (x == 15) = "quattuordecillion "
	| (x == 16) = "sexdecillion "
	| (x == 17) = "septendecillion "
	| (x == 18) = "octodecillion "
	| (x == 19) = "novemdecillion "
	| (x == 20) = "vigintillion "
	| (X == 21) = "unvigintillion"
	| otherwise = "out of scope number "