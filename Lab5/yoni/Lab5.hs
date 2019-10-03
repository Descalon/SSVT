module Lab5 where

import Data.List
import System.Random
import Lecture5
import Test.QuickCheck

genPos :: Gen Integer
genPos = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (> 0)

genListOfPos :: Gen [Integer]
genListOfPos = listOf genPos

--if exp `mod` 2 == 0 then expMEven x exp n else (expMEven x exp n) * x `mod` n


--expMEven :: Integer -> Integer -> Integer -> Integer
--expMEven x exp n = 0

slowExM :: Integer -> Integer -> Integer -> Integer
slowExM x e m = (x ^ e) `mod` m

exM :: Integer -> Integer -> Integer -> Integer
exM x e m = 
   let exM' c e' = if e' < e then exM' c' (e'+1) else c'
        where c' = (x * c) `mod` m
        in exM' 1 1

{--
exM  :: Integer -> Integer -> Integer -> Integer
exM base expo modules | expo == 0 = (rem 1 modules)
                        | modules == 1 = 0
                        | rem expo 2 == 1 = rem (rem ((rem base modules) * (exM base (expo - 1) modules)) modules) modules
                        | otherwise = rem ((exM  base (div expo 2) modules) * (exM base (div expo  2) modules)) modules
--}

exMTest :: Integer -> Integer -> Integer -> Bool
exMTest x e m = Lab5.exM x e m == Lecture5.exM x e m

exMTest2 :: Integer -> Integer -> Integer -> Bool
exMTest2 x e m = Lab5.exM x e m == (x ^ e) `mod` m

exMTest3  :: Integer -> Integer -> Integer -> Bool
exMTest3 x e m = Lab5.exM x e m > 0


composites :: [Integer]
composites = [x | x <- [1..], any(\y -> x `mod` y == 0) [1..x - 1]]


carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]


-- exerc 4

{--
4.2

Given Carmichael numbers share similar characteristics to the characteristics of the little Ferment theorem, the little Ferment theorem
fails on every carmichael number by stating it would in fact be a prime, where it's not. Carmichael numbers are always composite.

The characteristic;
if pc is a prime OR a carmichael number, then for any int b; b ^ pc - b is a multiple of pc. 
--}

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


-- exerc 6

tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function


--isValid :: Tree -> Bool
--isValid t = True

collect :: Tree a -> [a]
collect (T x ts) = x : concatMap Lab5.collect ts

-- {(x,y) | 1 ≤ x ≤ n, 1 ≤ y ≤ n with x, y co-prime}
--tree3 n = grow (step3 n) (1,1)
-- step3 n = \(x,y) -> if y <= n && x <= n && 1 <= x && 1<= y then [(x,y),(x, x + y)] else []

generateTestSet :: Integer -> [(Integer, Integer)]
generateTestSet n = [(x, y) | x <- [1..n], y <- [1..n], coprime x y]

coprimeTest1 :: Integer -> Bool
coprimeTest1 n = length x == length y && setContainsSet x y
        where x = (Lab5.collect $ tree1 n)
              y = (generateTestSet n)

coprimeTest2 :: Integer -> Bool
coprimeTest2 n = length x == length y && setContainsSet x y
        where x = (Lab5.collect $ tree2 n)
              y = (generateTestSet n)


setContainsSet :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
setContainsSet [] [] = True
setContainsSet (x:[]) (ys) = x `elem` ys
setContainsSet (x:xs) (ys) = x `elem` ys && setContainsSet xs ys