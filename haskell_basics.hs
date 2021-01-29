-- Korey Lombardi 
-- 1/28/2021
-- COP 4020 : Homework 1

module Hw1 where

-- 1. Fix the syntax errors in the program below, and test your solution using GHCi.
-- N = a 'div' length xs 
--     where
--        a = 10
--       xs = [1,2,3,4,5]
--
-- 1. 'N' is uppercase when it should be lowercase
-- 2. 'div' should be `div`
-- 3. xs is not aligned with a. To fix error add one more space before xs

divideByListSize :: Int -> Int
divideByListSize n = a `div` length xs
  where 
    a = 10
    xs = [1,2,3,4,5]


-- 2. Show how the library function last that selects the last element of a list can be defined using the
--    functions introduced in this lecture. For example:
--      > last [1,2,3,4,5]
--      > 5
--
-- Implementation below:
-- head (reverse [1,2,3,4,5])


-- 3. Show how the library function init that removes the last element from a list can be defined. For
--    example:
--             > init [1,2,3,4,5]
--             > [1,2,3,4]

init' :: [Int] -> [Int]
init' [] = []
init' [x] = []
init' (x:xs) = [x] ++ init' xs


-- 4. What are the types of the following values?
--    [’a’,’b’,’c’] = [Char]
--    (’a’,’b’,’c’) = (Char, Char, Char)
--    [(False,’0’),(True,’1’)] = [(Bool, Char)]


-- 5. What are the types of the following functions?
--    second xs = head (tail xs) = [a] -> a
--    swap (x,y) = (y,x) = (b, a) -> (a, b)
--    pair x y = (x,y) = a -> b -> (a, b)

