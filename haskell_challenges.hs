-- Haskell Challenges

module Haskell_Challenges where 

-- Give three possible definitions for the logical or operator (||) using pattern matching.
-- Example 1
or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 _ _         = True

-- Example 2
or2 :: Bool -> Bool -> Bool
or2 True _      = True
or2 _ True      = True
or2 False False = False

-- Example 3
or3 :: Bool -> Bool -> Bool
or3 True True   = True
or3 True False  = True
or3 False True  = True
or3 False False = False


-- Redefine the following version of (&&) using conditionals rather than patterns:
-- 		True && True = True
--		_    && _    = False
and' :: Bool -> Bool -> Bool
and' x y = if x == True
             then if y == True
               then True
               else False
             else False


-- A triple (x,y,z) of positive integers is called pythagorean if x2 + y2 = z2. 
-- Using a list comprehension, define a function
-- pyths :: Int -> [(Int, Int, Int)]
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], (x^2 + y^2 == z^2)]


-- A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
-- Using a list comprehension, define a function
-- perfects :: Int -> [Int]

-- First implement a function to extract all the factors for an input
get_factors :: Int -> [Int]
get_factors n = [x | x <- [1..(n `div` 2)], (n `mod` x == 0)]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (x == sum (get_factors x))]


-- The scalar product of two lists of integers xs and ys of length n is given by the sum of the products
-- Using a list comprehension, define a function that returns the scalar product of two lists.
scalar_prod :: [Int] -> [Int] -> Int
scalar_prod xs ys = sum [(x * y) | (x, y) <- zip xs ys]


-- Without looking at the standard prelude, define the following library functions using recursion:

-- and :: [Bool] -> Bool
and'' :: [Bool] -> Bool
and'' []     = True
and'' (x:xs) = x && and'' xs


-- Concatenate a list of lists:
-- concat :: [[a]] -> [a]
concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs


-- Produce a list with n identical elements:
-- replicate -> Int -> a -> [a]
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a


-- Select the nth element of a list:
-- (!!) :: [a] -> Int -> a
(!!!) :: [a] -> Int -> a
(!!!) [] _     = error "index too large"
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)


-- Decide if a value is an element of a list:
-- elem :: Eq a => a -> [a] -> Bool
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) | (y == x)  = True
               | otherwise = elem' y xs


-- Define a recursive function
-- that merges two sorted lists of values to give a single sorted list
-- merge :: Ord a => [a] -> [a] -> [a]
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge l1@(x:xs) l2@(y:ys)
  | (x <= y)  = x : merge xs l2
  | otherwise = y : merge l1 ys


-- Define merge sort
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge ys zs
             where 
               ys     = msort $ take middle xs
               zs     = msort $ drop middle xs
               middle = length xs `div` 2

