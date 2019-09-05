
module Lab1 where
import Data.List
import Data.Maybe
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

{--
-- Question 1. Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements.
--}

-- Exercise 2
sumQ1v1 :: Int -> Int
sumQ1v1 n = sum [x^2 | x <- [1..n]]

sumQ1v2 :: Int -> Int
sumQ1v2 n = (div)(n*(n+1)*(2*n+1))(6)

testQ1 :: Int -> Bool
testQ1 n = n>=1 --> sumQ1v1 n == sumQ1v2 n

-- Exercise 3
sumQ2v1 :: Int -> Int
sumQ2v1 n = sum [x^3 | x <- [1..n]]

sumQ2v2 :: Int -> Int
sumQ2v2 n = ((div)(n * (n+1))(2))^2

testQ2 :: Int -> Bool
testQ2 n = n>=1 --> sumQ2v1 n == sumQ2v2 n


{--
-- Question 2. Redo exercise 4
--}
lengthPSet1 :: Int -> Int
lengthPSet1 n = length (subsequences [1..n])

lengthPSet2 :: Int -> Int
lengthPSet2 n = 2^n

checkLengthPSet :: Int -> Bool
checkLengthPSet n = let a = abs n in lengthPSet1 a == lengthPSet2 a
{-- Answers Question 2
  Is this propery hard to test?
  It is. Especially when calculating lengths of powersets, it is easy to run into stack overflows.

  What are you actually testing?
  We are testing whether a list of possible combinations of set elements equals 2 ^ the number of elements in the set.
  This would count as a mathematical fact. Although the testing in inconclusive, because it cannot be completed due to
  software limitations.
--}

{--
-- Question 3. Redo exercise 5
--}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

hasSame :: (Eq a) => [a] -> [a] -> Bool
hasSame x y = null (x \\ y) && null (y \\ x)

testPerms :: Int -> Bool
testPerms n = n>=1 --> hasSame (perms [1..n]) (permutations [1..n])


{--
-- Question 4
--}
primestoN :: Integer -> [Integer]
primestoN n = 2 : filter prime [3..n]

primesto1000 :: [Integer]
primesto1000 = [x | x <- (primestoN 1000), elem (reversal x) (primestoN 1000)]

{-- Answers Question 4
  How would you test this function?
  By traversing the output and checking if for each element, the element is a prime and the reversal
  is also in the list. Then I would take each prime up to 1000 and make sure that where the prime has no
  reversal in the list, it is also not in the reversal primes list.
--}

{--
-- Question 5
--}
slice :: Int -> Int -> [Integer] -> [Integer]
slice from to xs = take (to - from + 1) (drop from xs)

findsumprimes :: [Integer] -> Integer
findsumprimes (x:xs) = if elem (sum (slice 0 100 (x:xs))) (x:xs) then sum (slice 0 100 (x:xs)) else findsumprimes xs
{-- Answers Question 5
  Do you have to test that your answer is correct? How could this be checked?
  You can check if the sum is actually a prime by using the prime function. You can also adjust to app to return the sum even it is not
  a prime. You can then test each ouput for whether it is a prima. The first one that return true should be the same answer
  as the above function.
--}

{--
-- Question 6
--}
exercise6:: [Integer] -> Int -> Integer
exercise6 (x:xs) n = if elem ((product (slice 0 n (x:xs))) + 1) (x:xs) == False then ((product (slice 0 n (x:xs))) + 1) else exercise6 (x:xs) (n+1)


{--
-- Question 7
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

isAmericanExpress, isMaster, isVisa :: Int -> Bool
isAmericanExpress n = (length (intToArray n) == 15) && (luhn n)
isMaster n = (length (intToArray n) == 16) && (luhn n)
isVisa n = (length (intToArray n) == 16) && (luhn n)


{--
-- Question 8
--}
data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]
honestPeople = 3

accuses :: Boy -> Boy -> Bool
accuses Matthew b = b /= Carl && b /= Matthew
accuses Peter b   = b == Matthew || b == Jack
accuses Jack b    = not (accuses Matthew b) && not (accuses Peter b)
accuses Arnold b  = matthewOrPeter && not matthewAndPeter
                      where
                        matthewOrPeter = (accuses Matthew b || accuses Peter b)
                        matthewAndPeter = (accuses Matthew b && accuses Peter b)
accuses Carl b    = not (accuses Arnold b)

accusers :: Boy -> [Boy]
accusers b = [x | x <- boys, accuses x b]

guilty, honest, lying :: [Boy]
guilty = [x | x <- boys, length(accusers x) == honestPeople]
honest = accusers (head guilty)
lying = [x | x <- boys, elem x honest == False]


main = do
  putStrLn ("Hey")
