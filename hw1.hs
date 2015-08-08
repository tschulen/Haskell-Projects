-- Tyler Schulenberg
-- CMPS 112
-- Haskell HW # 1

import Data.Char

-- 2
-- Write a function 
-- citeAuthor :: String -> String -> String
-- which puts first name and last name in reverse order:
-- citeAuthor "Herman" "Melville" -- -> "Melville, Herman"

citeAuthor :: String -> String -> String
citeAuthor x y = y ++ ", " ++ x

-- 3
-- Write a function
-- initials :: String -> String -> String
-- which returns the initials of the provided first name and last name:
-- initials "Herman" "Melville" -- -> "H.M."

initials :: String -> String -> String
initials x y = (head x:[]) ++ "." ++ (head y:[]) ++ "."

-- 4
-- Suppose that we represent books (author, title, year) as tuples (String, String, Int).
-- Write a function
-- title :: (String, String, Int) -> String
-- which returns the title of a book.
-- title ("Herman Melville", "Moby Dick", 1851) -- -> "Moby Dick"

title :: (String, String, Int) -> String
title (_,y,_) = y

-- 5
-- Write a function
-- citeBook :: (String, String, Int) -> String
-- which returns a citation in the format title (author, year)
-- citeBook ("Herman Melville", "Moby Dick", 1851) -- -> "Moby Dick (Herman Melville, 1851)"

citeBook :: (String,String,Int) -> String
citeBook (x,y,z) = y ++ " (" ++ x ++ ", " ++ show z ++ ")"

-- 6
-- Write a function
-- bibliography_rec :: [(String, String, Int)] -> String
-- which returns a string containing all the books as citations in the form returned by citeBook in part 5, separated by newlines. Use recursion and your previous citeBook function to build up the result.

bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = error "empty list"
bibliography_rec [(x,y,z)] = citeBook (x,y,z)
bibliography_rec((x,y,z):tail) = citeBook (x,y,z) ++ "\n" ++ bibliography_rec tail

-- 7
-- Write a function
-- bibliography_fold :: [(String, String, Int)] -> String
-- which does the same as bibliography_rec but instead of using recursion, it uses foldl to build up the string.

bibliography_fold :: [(String,String,Int)] -> String
bibliography_fold [] = error "empty list"
bibliography_fold xs = foldl(\d (a,b,c) -> d ++ " " ++ citeBook(a,b,c) ++ "\n ") [] xs

-- 8
-- Write a function
-- averageYear :: [(String, String, Int)] -> Int
-- which returns the average publication year of the provided books.
-- averageYear [("","",1),("","",3)] -- -> 2

averageYear :: [(String, String, Int)] -> Int
averageYear xs = div (sum(map year xs)) (length xs)

year :: (String, String, Int) -> Int
year (_,_,z) = z

-- 9
-- Write a function
-- references :: String -> Int
-- which takes a text with references in the format [n] and returns the total number of references.
-- txt :: String
-- txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
--       "get to their goal, and in the end the thing they want the most ends " ++
--       "up destroying them.  In case of [2] this is a whale...
-- references txt -- -> 3

references' :: String -> Int
references' x = length(filter (isRef) (words x))

isRef :: String -> Bool
isRef x
	| (((head x) == '[') && ((last x) == ']')) = True
	| otherwise = False
	
	
-- 10
-- Write a function
-- citeText :: [(String, String, Int)] -> String -> String
-- which takes a list of books and a text with references in the form [n] and returns a text with all references replaced by a citation of the n'th book using the citeBook function from problem 5.
-- let gatsby = ("F. Scott Fitzgerald", "The Great Gatsby", 1925)
-- let moby = ("Herman Melville", "Moby Dick", 1851)
-- citeText [gatsby, moby] txt
-- "The Great Gatsby (F. Scott Fitzgerald, 1925) and Moby Dick (Herman Melville, 1851) both feature..."


citeText :: [(String, String, Int)] -> String -> String	
citeText bib txt = replace bib (words txt)

replace :: [(String, String, Int)] -> [String] -> String
replace [] [] = ""
replace [] y = unwords y
replace _ [] = ""
replace bib (txtWord:tail) = if (isRef txtWord) 
					then citeBook (bib !! ((digitToInt(txtWord !! 1)) - 1)) ++ " " ++ replace bib tail
					else txtWord ++ " " ++ replace bib tail
				