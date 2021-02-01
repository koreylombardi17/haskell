--	first_script.hs
--	Korey Lombardi 
-- 	1/20/2021

module First_Script where 

-- Funtion to sum (50 + 50)

size :: Integer 
size = 50 + 50

-- Function to sqaure an Integer

square :: Integer -> Integer
square n = n*n

-- Function to double an Integer

double :: Integer -> Integer
double n = 2*n

-- Example using all 3 functions

example :: Integer
example = double (size - square (2+2))

-- Function to find the area of a triangle 
triArea :: Float -> Float -> Float -> Float
triArea a b c
  | possible  = sqrt(s*(s-a)*(s-b)*(s-c))
  | otherwise = 0
  where
    s        = (a+b+c)/2
    possible = ((a>0) && (b>0) && (c>0) && (a^2 + b^2 == c^2))
   
