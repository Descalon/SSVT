module Lab5 where

import Data.List
import System.Random
import Lecture5
import Test.QuickCheck

-- Exercise 1 -- ToDo
-- Performance Testing
-- Relevant QuickCheck Properties
-- Test Report
-- Implementation:
exM :: Integer -> Integer -> Integer -> Integer
exM x e m =
    let exM' c e' = if e' < e then exM' c' (e'+1) else c'
            where c' = (x * c) `mod` m
    in exM' 1 1

-- Exercise 2
-- Test report: using compositesTest checks whether results from composites are
-- not prime. Uses the prime function from Lecture5 module.
--
-- quickCheck compositesTest yield the following results:
--  +++ OK, passed 100 tests.
--
-- CompositesTest with values 500, 5000, 10000, 20000 and 100000 also all
-- yield True, so implementation looks correct.

composites :: [Integer]
composites = [x | x <- [1..], any (\y -> x `mod` y == 0) [2..(x - 1)]]

-- Checks whether first n results from composites function are not prime
compositesTest :: Int -> Bool
compositesTest n = all (\x -> (not . prime) x) (take n Lab5.composites)

-- Exercise 3
-- Test report: use testFermat with argument k to get the lowest number that
-- fools primeTestsF (primeTestsF says it is prime, while in fact it is not).
--
-- Least composite number that fools the check: Running testFermat 5 times: 
-- k = 1 yields 39, 33, 21, 15 and 28           average: 27.2
-- k = 2 yields 65, 259, 365, 1729 and 91       average: 501.8
-- k = 3 yields 703, 703, 1105, 15, 2821        average: 1069.4
-- k = 4 yields 8911, 2701, 15841, 1729, 2821   average: 6400.6
-- Higher values of k seems to make primeTestsF more accurate (the average
-- lowest erroneous value seems to get higher when using higher values of k).
testFermat :: Int -> IO Integer
testFermat k = testFermatHelper k 1

testFermatHelper :: Int -> Integer -> IO Integer
testFermatHelper k x = do
    b <- primeTestsF k x
    if b == True && x `elem` (take (fromIntegral x) Lab5.composites) then return x else testFermatHelper k (x + 1)
