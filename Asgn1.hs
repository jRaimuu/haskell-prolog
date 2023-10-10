{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


module Asgn1 where

import Prelude

data SF a = SS a | FF deriving (Eq, Show)

--Problem 1--
{-
    a)
    The function takes two Integers as parameter. It will compute the division between these two
    Intergers and return a variable wrapped in the SF data type. If it determines that the
    denominator y is zero, then it will retun FF, indicating that Integer division doesn't exist. 
    Otherwise, if the expression meets the criteria for integer division, it will return SS along
    with the result of the division
-}
sf_divide :: Integer -> Integer -> (SF Integer)
sf_divide x y
    | y == 0 = FF
    | otherwise = SS m
    where
        m = x `div` y 


{- 
    b)
    The function takes two integers as parameter. It will compute the remainder between the two
    integers and return the variable wrapped in an SF data type. If there is a remainder, 
    then SS is returned with the remainder value. Otherwise, FF is returned in the case of 
    a zero remainder.
 -}
sf_remainder :: Integer -> Integer -> (SF Integer)
sf_remainder x y
    | m /= 0 = SS m
    | otherwise = FF
    where
        m = x `rem` y


--Problem 2--

{-
    a)
-}

--Optimized
matches :: Integer -> [Integer] -> [Integer]
matches n [] = []
matches n (x:xs)
    | n == x    = x: matches n xs --head matches so we concat head with recursive call OR head matches and tail is empty (i.e. singleton), so [x] is returned
    | otherwise = matches n xs --head does not match so we recursively call with tail

{-
    b) Functionality is generally the same as part a but we return SS integer or FF accordingly  
-}
sf_matches :: Integer -> [Integer] -> (SF [Integer])
sf_matches n [] = FF --list is empty
sf_matches n (x:xs)
    | n == x = case sf_matches n xs of
                  SS rcsv -> SS (x:rcsv)
                  FF      -> SS [x] -- successful match with an empty list
    | otherwise = sf_matches n xs




--Problem 3
{-
    a) Two numbers are coprime if their greatest common divisor equals 1
-}
coprime :: Integer -> Integer -> Bool
coprime _ 0 = False --Zero  divisor
coprime x y
    | x < 0 || y < 0    = False -- Negative input
    | y == 1            = True --if remainder is 1 (meaning the two numbers are coprime), then return True 
    | x `mod` y == 0    = False --if zero remainder, return false
    | otherwise         = coprime y (x `mod` y) --otherwise, call coprime recursively with y and the remainder of x mod y


--Problem 4
{-
    a) 
    Source: https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind#Properties
            https://www.statisticshowto.com/stirling-numbers-second-kind/

    The function takes two integers as parameter. It will compute the sterling number S(m, n) between the two
    integers and return the variable wrapped in an SF data type. Where:
    S(m, n) evulates the number of ways to partition a set of m objects into n non-empty subsets
    
    Below are 2 base cases that are pattern maatched:
        - if no non-empty subsets then return success wrapping integer 0
        - if no objects to partition then also return success weapping integer 0
    
    Next is a sequence of guards that the check boolean expressions:
        - if the # of objects is not a positive integer, then return FF
        - if m == n, then there is only one way to partion m objects into n subsets

    Otherwise, we show Success with the recursively computed sterling number using the formula:
    stirling_number_2nd (m - 1) (n - 1) + n * stirling_number_2nd(m - 1) n

    It is necessary to provide the "where" construct because the whole expression itself couldn't return
    as an iteger. So the where is utlized to pattern match the result of the recursive call and derive 
    the necessary values, binding them to rcsv1 or rcsv2 accordingly so the real values can be used in 
    the expression

-}
stirling_number_2nd :: Integer -> Integer -> (SF Integer)
stirling_number_2nd m 0 = SS 0
stirling_number_2nd 0 n = SS 0
stirling_number_2nd m n
    | m < 0     = FF
    | m == n    = SS 1
    | otherwise = SS (rcsv1 + n * rcsv2)
        where
            SS rcsv1 = stirling_number_2nd (m - 1) (n - 1)
            SS rcsv2 = stirling_number_2nd(m - 1) n


{-
    b)
    Source: https://en.wikipedia.org/wiki/Bell_number

    The function takes one integer as parameter. It will compute the Bell number B(n) and return the variable 
    wrapped in an SF data type.
    
    nth_bell_number Base cases:
        - Zeroth bell number is 1, so return SS 1
        - n must be a positive integer, return FF in this case

    Otherwise, call the helper function to calculate the running total
        
    calc_bell_total Base cases:
        - if the k exceeds n (so that all partition sizes have been considerd) return SS accompanied by 
            the running total

    Otherwise, we use the stirling_number_2nd helper function
    
    In the case that stirling_number_2nd returns SS with var, we recursively calculate the Sterling number
    at each partition size by adding var (current sterling number) to the total.


-}
nth_bell_number :: Integer -> (SF Integer)
nth_bell_number 0 = SS 1
nth_bell_number n
    | n < 0     = FF
    | otherwise = calc_bell_total n 1 0


calc_bell_total :: Integer -> Integer -> Integer -> (SF Integer)
calc_bell_total n k total
    | k > n     = SS total
    | otherwise = case stirling_number_2nd n k of
                    SS var -> calc_bell_total (n) (k + 1) (total + var)
                    FF   -> FF

