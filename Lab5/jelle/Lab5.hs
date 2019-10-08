module Lab5 where

import Data.List
import System.Random
import Lecture5
import Test.QuickCheck

-- Assignment 1
exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM 0 _ _ = 0
exM _ _ 0 = 0
exM x y m
    | isPowerOf2 y = computeMod x y m
    | otherwise    = ((Lab5.exM x greatestPower m) * (Lab5.exM x (y - greatestPower) m)) `mod` 5
        where
            greatestPower = getGreatestPowerOf2 y 0

computeMod :: Integer -> Integer -> Integer -> Integer
computeMod x y m
    | y == 1    = x `mod` m
    | otherwise = computeMod ((x `mod` m)^2) (y `div` 2) m

-- Accumulator (=second) argument should be 0 at start
getGreatestPowerOf2 :: Integer -> Integer -> Integer
getGreatestPowerOf2 0 acc = acc
getGreatestPowerOf2 1 acc = acc
getGreatestPowerOf2 x acc = if x `mod` 2 == 0 then getGreatestPowerOf2 (x `div` 2) (acc + 1) else getGreatestPowerOf2 ((x - 1) `div` 2) (acc + 1)

isPowerOf2 :: Integer -> Bool
isPowerOf2 x = if x - (2 ^ (getGreatestPowerOf2 x 0)) == 0 then True else False

checkExM :: Integer -> Integer -> Integer -> Bool
checkExM 0 _ 0 = True
checkExM _ _ 0 = True
checkExM _ 0 _ = True
checkExM x y m = Lab5.exM x y m == expM x y m

-- Assignment 2
-- Test report: using compositesTest checks whether results from composites are
-- not prime. Uses the prime function from Lecture5 module. CompositesTest with
-- values 500, 5000, 10000, 20000 and 100000 all yield True, so implementation
-- seems correct.
composites :: [Integer]
composites = [x | x <- [1..], any (\y -> x `mod` y == 0) [2..(x - 1)]]

-- Checks whether first n results from composites function are not prime
compositesTest :: Int -> Bool
compositesTest n = all (\x -> (not . prime) x) (take n Lab5.composites)

-- Assignment 3
-- Test report: use testFermat with argument k to get the lowest number that
-- fools primeTestsF (primeTestsF says it is prime, while in fact it is not).
-- running testFermat 5 times with k = 1 yields 39, 33, 21, 15 and 28, average
-- of 27.2. The same with k = 2 yields 65, 259, 365, 1729 and 91, average of
-- 501.8. k = 3 yields 703, 703, 1105, 15, 2821, average of 1069.4. The same
-- with k = 4 yields 8911, 2701, 15841, 1729, 2821, average of 6400.6.
-- Higher values of k seems to make primeTestsF more accurate (the average
-- lowest erroneous value seems to get higher when using higher values of k).
testFermat :: Int -> IO Integer
testFermat k = testFermatHelper k 1

testFermatHelper :: Int -> Integer -> IO Integer
testFermatHelper k x = do
    b <- primeTestsF k x
    if b == True && x `elem` (take (fromIntegral x) Lab5.composites) then return x else testFermatHelper k (x + 1)

-- Assignment 4
-- The Miller-Rabin primality check with Carmichael numbers keeps running, which
-- might be an indication that it is more accurate than the Fermat function: no
-- Carmichael number is found which passes the primeMR function. The Fermat
-- test, however, fails at the first Carmichael number, and also subsequent
-- Carmichael numbers. This matches the information on Wikipedia: Carmichael
-- numbers are never prime, but will always pass the Fermat algorithm as prime.

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1) ]

testFermatCarmichael :: Int -> IO Integer
testFermatCarmichael k = testFermatCarmichaelHelper k 0

testFermatCarmichaelHelper :: Int -> Integer -> IO Integer
testFermatCarmichaelHelper k x = do
    b <- primeTestsF k (carmichael !! (fromIntegral x))
    if b == True then return (carmichael !! (fromIntegral x)) else testFermatCarmichaelHelper k (x + 1)


testMRCarmichael :: Int -> IO Integer
testMRCarmichael k = testMRCarmichaelHelper k 0

testMRCarmichaelHelper :: Int -> Integer -> IO Integer
testMRCarmichaelHelper k x = do
    print(carmichael !! (fromIntegral x))
    b <- primeMR k (carmichael !! (fromIntegral x))
    if b == True then return (carmichael !! (fromIntegral x)) else testMRCarmichaelHelper k (x + 1)


-- Assignment 5
-- produceMersenne randomly checks of one of the first 9 primes whether the 
-- result of 2 ^ p - 1 is also prime. If they are also prime, then they are 
-- mersenne primes. This is checked with the website 
-- http://mathworld.wolfram.com/MersennePrime.html, which lists the first few
-- Mersenne primes.
-- Picking a prime above the first 9 makes the program take too much time, so
-- these are not checked.
-- The PrimeIndex variant of the function takes one argument: the index of the
-- in the list of primes produced by the primes function. It then checks whether
-- 2 ^ p - 1 is prime as well.
-- the second function with argument 0 (checking the first prime, 2) yields
-- True, argument 1 (checking the second prime, 3) yields True. Running it with
-- all arguments [0..9] yields 7 times True, 3 times False as results. All
-- cases that yield True produce a mersenne prime (checked with the earlier
-- mentioned website). So for the tested cases, 70% are mersenne primes.
produceMersenne :: IO Bool
produceMersenne = do
    r <- randomRIO (0,8)
    putStrLn ("Prime: " ++ (show (primes !! r)))
    putStrLn ("Also prime? : " ++ (show ((2 ^ (primes !! r)) - 1)))
    (primeMR 1 ((2 ^ (primes !! r)) - 1))

produceMersennePrimeIndex :: Int -> IO Bool
produceMersennePrimeIndex x = do
    putStrLn ("Prime: " ++ (show (primes !! x)))
    putStrLn ("Also prime? : " ++ (show ((2 ^ (primes !! x)) - 1)))
    (primeMR 1 ((2 ^ (primes !! x)) - 1))



-- Assignment 6: the compareCollectList functions compares a list comprehension
-- to collected items from the tree. It uses a collect function to get all the
-- elements from the tree, and a list comprehension function that gets all the
-- elements in the provided list comprehension.
-- The testTree functions can be used to determine whether for all values up to
-- a given n the compareCollectList functions return True. This checks whether
-- the statements in question 1 and 2 are valid. Running testTree1 and testTree2
-- with an n value of 200 both result in True, so the statements are probably
-- correct. (The compareCollectList functions are called with arguments from 1
-- up to the n value given to testTree, so they are not just checked with a
-- single n.)
tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

treeCollect :: Tree a -> [a]
treeCollect (T x ts) = x : concatMap treeCollect ts

treeListComp :: Integer -> [(Integer, Integer)]
treeListComp n = [(x,y) | x <- [1..n], y <- [1..n], coprime x y]

compareCollectList1 :: Integer -> Bool
compareCollectList1 n = (sortPairs . treeCollect) (tree1 n) == (sortPairs . treeListComp) n

compareCollectList2 :: Integer -> Bool
compareCollectList2 n = (sortPairs . treeCollect) (tree2 n) == (sortPairs . treeListComp) n

sortPairs :: [(Integer,Integer)] -> [(Integer,Integer)]
sortPairs xs = sortBy comparePairs xs

comparePairs :: (Integer,Integer) -> (Integer,Integer) -> Ordering
comparePairs (a,b) (c,d)
    | a < c     = LT
    | a > c     = GT
    | b < d     = LT
    | b > d     = GT
    | otherwise = EQ

testTree1 :: Integer -> Bool
testTree1 n = all compareCollectList1 [1..n]

testTree2 :: Integer -> Bool
testTree2 n = all compareCollectList2 [1..n]
