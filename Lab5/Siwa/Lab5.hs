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

-- expM 4 2 12 = 4. Eigenlijk is het (4^2) mod 12
-- exm :: Integer -> Integer -> Integer -> Integer
-- exm x 0 n = 1
-- exm x 1 n = x `mod` n
-- exm x e n = if even e then exm ((x*x) `mod` n) (e `div` 2) n else
--             x * (exm ((x*x) `mod` n) ((e-1) `div` 2) n)

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
