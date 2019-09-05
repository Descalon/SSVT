module Lab1 where
import Data.List
import Data.Char
import Test.QuickCheck    

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

-- setting up, took about 10min
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

inductionOne :: Integer -> Integer
inductionOne n = sum [k ^ 2 | k <- [1..n]]

inductionTwo :: Integer -> Integer
inductionTwo n = (n * (n + 1) * (2 * n + 1)) `div` 6

testWorkTwo :: Integer -> Bool
testWorkTwo n = n>=0 --> inductionOne n == inductionTwo n

-- Second part took 4 minutes, missing a ] for a minute
inductionThree :: Integer -> Integer
inductionThree n = sum [k ^ 3 | k <- [1..n]]

inductionFour :: Integer -> Integer
inductionFour n = ((n * (n + 1)) `div` 2) ^ 2

testWorkTwoPartTwo :: Integer -> Bool
testWorkTwoPartTwo n = n>=0 --> inductionThree n == inductionFour n


-- Exercise 2, 15min due to missing essential stuff, test takes too long

-- this provides us with the length of the power set of a
induction2 :: [a] -> Int
induction2 a = length (subsequences a)


-- Takes too long
induction2Test :: Integer -> Bool
induction2Test n = n>=0 --> induction2 [1..n] == 2 ^(length [1..n])

{--

We test part of the specification provided, however, the lists are created while testing and therefore the lists can be extremely big, making the test hang on for quite a while.
--}

-- exercise 3
-- This one took quite a bit, but mainly due to syntax error
-- testing here takes way too long as well, we are testing a math fact and specification

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

exerciseThreeTest :: [Integer] -> Bool
exerciseThreeTest n = product [1..(length n)] == length (perms n)


-- exercise 4
-- Took 10-15min, had to look up filter syntax

filterReversablePrimes :: [Integer] -> [Integer]
filterReversablePrimes a = filter (\x -> prime x && prime (reversal x)) a

-- Testing this would be hard, although by iterating through the first 100 primes it would provide with some sense of proof
-- Here again it wouldn't make sense to test for very big numbers as it would simply use prime generation for 1 to that point


-- exercise 5
-- 50min? figuring out loop recursion, slicing idea provided by Jelle
get101PrimesByIndex :: Int -> [Integer]
get101PrimesByIndex x = take 101 $ drop x $ primes

-- starting at index 0 ?
exercise5 :: Int -> Integer
exercise5 x = if prime (sum(get101PrimesByIndex x)) then sum(get101PrimesByIndex(x)) else exercise5 (x + 1)

-- Testing this could be done manually, but that'd be crazy. I replaced the 101 by 5 and got 101 as answer, just like the exercise
-- mentioned itself

-- exercise 6
-- 15min, took a silly amount of time to realise I had to flip the then and else
exercise6 :: Int -> Integer
execise6 0 = exercise6 1
exercise6 x = if prime(product(take x primes) + 1) then exercise6 (x + 1) else (product(take x primes) + 1)


-- exercise 7
-- 11:46 - 13:20
{--
luhnDouble :: Integer -> Integer
luhnDouble x = if x * 2 > 9 then x * 2 - 9 else x * 2

doubleEvenElements :: [Integer] -> [Integer]
doubleEvenElements [] = []
doubleEvenElements [x] = [x]
doubleEvenElements (x:y:xs) = (x):(if y * 2 > 9 then y* 2 - 9 else y * 2):(doubleEvenElements xs)

luhn :: Integer -> Integer
luhn x = sum (map toInteger (doubleEvenElements (map toInteger (map digitToInt (show (reversal (x)))))))
--}

{--
I got very much distracted by calculating the check digit rather than validating, therefore I sat down with
Siwa to see his approach and understood it clearly. I will keep the digit generation more or less up here
for future reference

Finalised the card type check later on myself
--}

intToArray :: Int -> [Int]
intToArray 0 = []
intToArray x = intToArray (x `div` 10) ++ [x `mod` 10]

codeToArray :: Int -> [Int]
codeToArray x = reverse (intToArray x)

luhnDouble :: Int -> Int
luhnDouble n = if n*2 > 9 then n*2 - 9 else n*2

dupeven :: [Int] -> [Int]
dupeven [] = []
dupeven (x:xs) = (luhnDouble(x)) : dupodd xs

dupodd :: [Int] -> [Int]
dupodd [] = []
dupodd (x:xs) = x : dupeven xs

luhn :: Int -> Bool
luhn x = if (mod)(sum (dupodd (codeToArray x))) (10) == 0 then True else False


-- Apparently mastercards ALWAYS start with a 5 and a visa with a 4. length of Amurican express is 15 at all times
isAmericanExpress, isMaster, isVisa :: Int -> Bool
isAmericanExpress n = (length (intToArray n) == 15) && (luhn n)
isMaster n = (length (intToArray n) == 16) && (luhn n) && (show n !! 0) == '5'
isVisa n = (length (intToArray n) == 16) && (luhn n) && (show n !! 0) == '4'


-- Exercise 8
-- 13:40 - 14:46

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x


accuses :: Boy -> Boy -> Bool
accuses Matthew Peter = True
accuses Matthew Jack = True
accuses Matthew Arnold = True
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Jack x = not(accuses Matthew x) && not(accuses Peter x)
accuses Arnold x = (accuses Matthew x) `xor` (accuses Peter x) 
accuses Carl x = not(accuses Arnold x)
accuses _ _ = False

accusers :: Boy -> [Boy]
accusers b = filter (\x -> accuses x b) boys


-- Since only 3 can be telling the truth
guiltyBoy = head $ filter(\x -> length (accusers x) >= 3) boys

-- 3 spoke the truth as they all said the name of the guilty boy
honestBoys = accusers guiltyBoy


