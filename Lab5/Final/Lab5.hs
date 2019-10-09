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

-- Return list of composite integers
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

testPrimeTestsF :: [Integer] -> Int -> IO()
testPrimeTestsF (x:xs) k = do
                            result <- primeTestsF k x
                            if (result == True) then print(x) else
                              testPrimeTestsF xs k

-- Exercise 4
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1) ]

-- Answers to the questions
-- 4.1
-- We find that that testPrimeTestsF tends return the head of the carmichael lists, even though it is a composite.
-- 4.2
-- Given Carmichael numbers share similar characteristics to the characteristics
-- of the little Ferment theorem, the little Ferment theorem fails on every
-- carmichael number by stating it would in fact be a prime, where it's not.
-- Carmichael numbers are always composite. The characteristic; if pc is a
-- prime OR a carmichael number, then for any int b; b ^ pc - b is a multiple of pc.
-- 4.3
-- We find that the testPrimeMR never returns a value. This is due to the fact that
-- primeMR always returns false.
-- Tests
-- 4.1
-- Performse testPrimeTestsF with carmichael numbers.
testPrimeTestsFCarmichael k = testPrimeTestsF carmichael k
-- 4.3
-- Tests the primeMR with an array of integers by choise.
testPrimeMR :: Int -> [Integer] -> IO()
testPrimeMR _ []    = print("Finished Test")
testPrimeMR k (x:xs)= do
                      result <- primeMR k x
                      if (result == True) then print(x) else
                        testPrimeMR k xs

-- Exercise 5 -- ToDo

-- Exercise 6
-- Answers to the questions:
-- It is possible to test that both sets contain the same elements. We do this/
-- in the tests below where we construct both sets (for each question) using the properties
-- described. It is then possible to check whether the elements of the first set are also in the second
-- and then to check whether the elements of the second set are also in the first.

-- Question 1
tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

-- Question 2
tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

-- Tests
collect :: Tree a -> [a]
collect (T x ts) = x : concatMap Lab5.collect ts

-- create a set where x and y are coprime
generateTestSet :: Integer -> [(Integer, Integer)]
generateTestSet n = [(x, y) | x <- [1..n], y <- [1..n], coprime x y]

-- quickCheck 'helper' function to test tree1 in comparison to
-- generateTestSet
coprimeTest1 :: Integer -> Bool
coprimeTest1 n = length x == length y && setContainsSet x y
        where x = (Lab5.collect $ tree1 n)
              y = (generateTestSet n)

-- quickCheck 'helper' function to test tree2 in comparison to
-- generateTestSet
coprimeTest2 :: Integer -> Bool
coprimeTest2 n = length x == length y && setContainsSet x y
        where x = (Lab5.collect $ tree2 n)
              y = (generateTestSet n)

-- compare sets iteratively, making sure all combinations in xs are found
-- in ys
setContainsSet :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
setContainsSet [] [] = True
setContainsSet (x:[]) (ys) = x `elem` ys
setContainsSet (x:xs) (ys) = x `elem` ys && setContainsSet xs ys

-- Test Report
-- Question 1 test return:
-- quickCheck $ forAll genPos $ coprimeTest1
--  +++ OK, passed 100 tests.
-- Question 2 test return:
-- quickCheck $ forAll genPos $ coprimeTest2
--  +++ OK, passed 100 tests.
