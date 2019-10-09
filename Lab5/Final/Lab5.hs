module Lab5 where

import Data.List
import System.Random
import Lecture5
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Bits
import Text.Printf
import System.CPUTime

{-- Exercise 1
    Main implementation based on material from Khan Academy (https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation)
    Performance tested by using the testPerformances function and enabeling the GHCI statistics (:set +s). The results can vary wildly and given is a list of tests run:

    components (x, e, m) > 1000
    slowExM : 9.984s,  10.094s, 10.828s
    exM     : 10.469s,  9.938s, 10.781s
    exM'    : 10.484s, 10.391s, 10.547s

    comps > 100
    slowExM : 171.875ms, 140.625ms, 109.375ms
    exM     : 125.000ms, 109.375ms, 156.250ms
    exM'    : 140.625ms, 125.000ms, 187.500ms

    
--}

(%) :: Integer -> Integer -> Integer
a % b = a `mod` b

slowExM :: Integer -> Integer -> Integer -> Integer
slowExM x e m = (x ^ e) % m

exM :: Integer -> Integer -> Integer -> Integer
exM x e m = 
    let f c e' = if e' < e then f c' (e'+1) else c'
            where c' = (x * c) % m
    in f 1 1

getBitSize :: Integer -> Int
getBitSize x = floor $ logBase 2 $ fromIntegral x

collectSquares :: Integer -> Integer -> Integer -> [Integer]
collectSquares x e m = 
    let f x' e' = if e' < e then c : f c (e' * 2) else [c]
            where c = (x' * x') % m
        first = x % m
    in first : (f x 2)

selectSquares :: [Integer] -> Integer -> Int -> [Integer]
selectSquares _ _ (-1) = []
selectSquares [] _ _ = []
selectSquares (sq:sqs) x b = if testBit x b then
                                sq : selectSquares sqs x (b-1)
                                else
                                selectSquares sqs x (b-1)

exM' :: Integer -> Integer -> Integer -> Integer
exM' x e m = x' % m
    where 
        l = floor $ logBase 2 (fromIntegral e) 
        sqs = reverse $ collectSquares x (2^l) m
        x' = product $ selectSquares sqs e l

clamp = 100
genPos :: Gen (Integer, Integer, Integer)
genPos = (arbitrary :: Gen (Integer,Integer,Integer)) `suchThat` (\ (x,y,z) -> x > clamp && y > clamp && z > clamp)

genList :: Gen [(Integer, Integer, Integer)]
genList = listOf genPos

wrapper f (x,e,m) = (f x e m) >= 0
    
diffMs :: Integer -> Integer -> Float
diffMs s e = (fromIntegral (e - s)) / (10 ^ 9)

testPerformance :: (Integer -> Integer -> Integer -> Integer) -> IO Float
testPerformance f = do
    s <- getCPUTime
    quickCheck $ forAll genPos $ \ (x,e,m) -> (f x e m) >= 0
    e <- getCPUTime
    let d = diffMs s e
    return d

printResult :: String -> Float -> IO ()
printResult s d = do
    printf s
    printf "\nRan for %0.3f ms\n" d
    return ()

testPerformances :: IO ()
testPerformances = do
    d1 <- testPerformance slowExM
    printResult "slowExM" d1
    d2 <- testPerformance exM
    printResult "exM" d2
    d3 <- testPerformance exM'
    printResult "exM2" d3
    
    return ()
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

-- Exercise 5
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
