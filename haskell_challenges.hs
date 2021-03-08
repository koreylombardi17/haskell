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


-- Function that searchs for a value in a tree
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)      = (compare x y == EQ)
occurs' x (Node l y r) | (compare x y == EQ) = True
                       | (compare x y == LT) = (occurs' x l)
                       | otherwise           = (occurs' x r)


-- Extend the abstract machine to support the use of multiplication.
-- Abstract Machine:

data Expr = Val Int | Add Expr Expr | Mult Expr Expr
type Cont = [Op]
data Op = EVAL Expr | ADD Int | MULT Int

eval :: Expr -> Cont -> Int
eval (Val n) c    = exec c n
eval (Add x y) c  = eval x (EVAL y : c)
eval (Mult x y) c = eval x (EVAL y : c)
-- eval evaluates an expression in the context of a control stack. That is, if the expression is an integer, it is
-- already fully evaluated, and we begin executing the control stack. If the expression is an addition, we
-- evaluate the first argument, x, placing the operation EVAL y on top of the control stack to indicate that
-- the second argument, y, should be evaluated once evaluation of the first argument is completed.

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m  = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)
-- exec executes a control stack in the context of an integer argument. That is, if the control stack is empty,
-- we return the integer argument as the result of the execution. If the top of the control stack is an
-- operation EVAL y, we evaluate the expression y, placing the operation ADD n on top of the remaining
-- stack to indicate that the current integer argument, n, should be added together with the result of
-- evaluating y once this is completed. And finally, if the top of the stack is an operation ADD n, evaluation
-- of the two arguments of an addition expression is now complete, and we execute the remaining control
-- stack in the context of the sum of the two resulting integer values.

value :: Expr -> Int
value e = eval e []

-- Define an action adder :: IO () that reads a given number of integers from the key board, one per
-- line, and displays their sum. For example:
adder :: IO ()
adder =  do putStr "How many numbers do you want to add?"
            size <- getLine
            numbers <- get_numbers (read size :: Int)
            putStrLn ("Total = " ++ show (sum numbers))


get_numbers        :: Int -> IO [Int]
get_numbers 0      =  return []
get_numbers index  =  do
                         number_as_string <- getLine
                         let number = read number_as_string :: Int
						 numbers <- get_numbers (index - 1)
                         return (number : numbers)

-- Define a program
-- fibs :: [Integer]
-- that generates the infinite Fibonacci sequence
-- [0,1,1,2,3,5,8,13,21,34, …
-- using the following procedure:
-- a) The first two numbers are 0 and 1;
-- b) The next is the sum of the previous two;
-- c) Return to step 2.
-- II. Define a function
-- fib :: Int -> Integer
-- that calculates the nth Fibonacci number.
fibs :: [Integer]
fibs = 0:1:[ first + second | (first, second) <- zip fibs (tail fibs)]


fib       :: Int -> Integer
fib index = fibs !! (index - 1)
