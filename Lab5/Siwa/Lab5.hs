module Lab5 where

import Data.List
import System.Random
import Lecture5
import Test.QuickCheck

import Criterion.Measurement


-- main = secs <$> time_ exM' 58 131 15 >>= print


{--
Exercise 1
Performance Testing
Relevant QuickCheck Properties
Test Report
Implementation:
--}
exM :: Integer -> Integer -> Integer -> Integer
exM x e m =
    let exM' c e' = if e' < e then exM' c' (e'+1) else c'
            where c' = (x * c) `mod` m
    in exM' 1 1


-- Exercise 2
composites :: [Integer]
composites = [x | x <- [1..], not (prime x)]

testPrimeTestF :: [Integer] -> IO()
testPrimeTestF (x:xs) = do
                          result <- primeTestF x
                          if (result == True) then print(x) else
                            testPrimeTestF xs
-- Exercise 3
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

-- 4.1
testPrimeTestsFCarmichael k = testPrimeTestsF carmichael k

{-- 4.2
All carmichael numbers are known to be composites. That can be seen in
in the definition of the function. We also know that all carmichael numbers
are known to pass Fermat's Primality Check.
--}

-- 4.3
testPrimeMR :: Int -> [Integer] -> IO()
testPrimeMR _ []    = print("Finished Test")
testPrimeMR k (x:xs)= do
                      result <- primeMR k x
                      if (result == True) then print(x) else
                        testPrimeMR k xs


-- exerc 5
-- produceMersenne randomly checks one of the first 9 primes whether the result
-- of 2 ^ p - 1 is also prime. If they are also prime, then they are mersenne
-- primes. This is checked with the website
-- http://mathworld.wolfram.com/MersennePrime.html, which lists the first few
-- Mersenne primes.
-- Picking a prime above the first 9 makes the program take too much time, so
-- these are not checked.

produceMersenne :: IO Bool
produceMersenne = do
    r <- randomRIO (0,8)
    putStrLn ("Prime: " ++ (show (primes !! r)))
    putStrLn ("Also prime? : " ++ (show ((2 ^ (primes !! r)) - 1)))
    (primeMR 1 ((2 ^ (primes !! r)) - 1))

-- Exercise 6
genPos :: Gen Integer
genPos = (arbitrary :: Gen Integer) `suchThat` (> 0)

collectElements :: Tree a -> [a]
collectElements (T x ts) = x : concatMap collectElements ts

generateCompleteSet :: Integer -> [(Integer,Integer)]
generateCompleteSet n = [(x,y) | x <- [1..n], y <- [1..n], coprime x y]

-- 6.1
tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else []

compareTree1ToSet :: Integer -> Bool
compareTree1ToSet n = null ((generateCompleteSet n) \\ (collectElements (tree1 n))) && null ((collectElements (tree1 n)) \\ (generateCompleteSet n))

finalTestTree1 = quickCheck $ forAll genPos $ compareTree1ToSet

-- 6.2
tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

compareTree2ToSet :: Integer -> Bool
compareTree2ToSet n = null ((generateCompleteSet n) \\ (collectElements (tree2 n))) && null ((collectElements (tree2 n)) \\ (generateCompleteSet n))

finalTestTree2 = quickCheck $ forAll genPos $ compareTree2ToSet
