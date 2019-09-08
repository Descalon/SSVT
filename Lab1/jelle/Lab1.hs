-- Jelle Witsen Elias
-- Software specification, verification and testing, week 1
-- Software Engineering
-- 05-09-2019


module Lab1 where
import Data.List
import Test.QuickCheck  
import Data.Char  

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]



-- Exercise 1: 20 mins
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> suchThat (arbitrary :: Gen Integer) (> 0)

induction1' :: Integer -> Integer
induction1' n = sum [ k^2 | k <- [1..n] ]

induction1 :: Integer -> Integer
induction1 n = div (n * (n + 1) * (2 * n + 1)) 6

testInduction1 :: Integer -> Bool
testInduction1 n = induction1' n == induction1 n


induction2' :: Integer -> Integer
induction2' n = sum [ k^3 | k <- [1..n] ]

induction2 :: Integer -> Integer
induction2 n = ((n*(n+1)) `div` 2)^2

testInduction2 :: Integer -> Bool
testInduction2 n = induction2' n == induction2 n


-- Exercise 2: 20 mins
-- Hard to test! A lot of subsequences for 30+ elements in a list
-- Testing a mathematical fact (assuming subsequences works properly)
length1 :: [a] -> Int
length1 xs = length (subsequences xs)

length2 :: [a] -> Int
length2 xs = length xs

testLength :: [a] -> Bool
testLength xs = length1 xs == 2 ^ (length2 xs)


-- Exercise 3: 30 mins
-- Hard to test. Probably because calculating all permutations of large list
-- would become a huge calculation. For instance, a list with 20 elements would
-- have a permutations list with 20! elements, which is a huge number.
-- You're both testing whether terms is correct and if so, whether the
-- mathematical fact is true.
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

testPerms :: [Int] -> Bool
testPerms xs = product [1..(length xs)] == length (perms xs)

-- Exercise 4: 15 mins
-- Perhaps test this by making a different implementation, and check whether the
-- results are the same?
getReversePrimes :: [Integer]
getReversePrimes = filter (\x -> prime x && prime (reversal x)) [1..10000]

-- Exercise 5: 60 mins
-- Try with a small number (instead of 101) and check by hand whether result is 
-- correct.
getConsPrime :: Int -> Integer
getConsPrime x = sum (take 101 $ drop x $ primes)

getFirstConsPrime :: Int -> Integer
getFirstConsPrime x = if prime (getConsPrime x) then getConsPrime x else getFirstConsPrime (x + 1)

excercise5 :: Integer
excercise5 = getFirstConsPrime 0

-- Exercise 6: 30 mins
-- The smallest counterexample is 30031
getPrimesProduct :: Int -> Integer
getPrimesProduct x = (product(take x primes)) + 1

counterExamples :: Int -> Integer
counterExamples x = if prime (getPrimesProduct x) then counterExamples (x + 1) else (getPrimesProduct x)

excercise6 :: Integer
excercise6 = counterExamples 0


-- Exercise 7: 90 mins
luhn :: Integer -> Bool
luhn x = digitsSum (read (intToLuhnString x)) `mod` 10 == 0

digitsSum :: Integer -> Int
digitsSum = sum . map digitToInt . show

intToLuhnString :: Integer -> [Char]
intToLuhnString x = editString (show x) (length (show x) - 2)

editString :: [Char] -> Int -> [Char]
editString xs n = if n > 1 then (editString (take n xs) (n - 2)) ++ (updateElement (xs !! n)) ++ drop (n + 1) xs
                  else take n xs ++ (updateElement (xs !! n)) ++ drop (n + 1) xs

updateElement :: Char -> [Char]
updateElement x = show (if digitToInt x * 2 <= 9 then digitToInt x * 2 else addDigits ((digitToInt x) * 2))

addDigits :: Int -> Int
addDigits x = x `mod` 9


isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = (length (show n) == 15) && (luhn n)

isMaster n = (length (show n) == 16) && (show n !! 0) == '5' && (luhn n)

isVisa n = (length (show n) == 16) && (show n !! 0) == '4' && (luhn n)


-- Exercise 8: 90 mins
xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

accuses :: Boy -> Boy -> Bool
accuses Matthew b = b /= Carl && b /= Matthew
accuses Peter b = b == Matthew || b == Jack
accuses Jack b = not (accuses Matthew b) && not (accuses Peter b)
accuses Arnold b = accuses Matthew b `xor` accuses Peter b
accuses Carl b = not (accuses Arnold b)

accusers :: Boy -> [Boy]
accusers b = [x | x <- boys, accuses x b]

guilty, honest :: [Boy]
guilty = [x | x <- boys, length (accusers x) == 3] 

honest = [x | x <- boys, accuses x Jack]