import Data.List

-- Helper functions
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
            where 
            xs = takeWhile (\ y -> (y ^ (2 :: Integer)) <= n) primes

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

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]
-- / Helper functions

-- Exercise 1 (15 min)
lhs :: Integer -> Integer
lhs n = sum [x ^ 2 | x <- [1..n]]

rhs :: Integer -> Integer
rhs n = ( n*(n+1)*(2*n+1) ) `div` 6

test n = lhs x == rhs x
    where
        x = abs n
        

lhs' :: Integer -> Integer
lhs' n = sum [x ^ 3 | x <- [1..n]]

rhs' :: Integer -> Integer
rhs' n = ((n * (n + 1)) `div` 2)^2

test' n = lhs x == rhs x
        where
            x = abs n

--Exercise 2 (20 min)
set :: Integer -> [Integer]
set 0 = []
set x = [0..x]

powerSet :: [Integer] -> [[Integer]]
powerSet ns = subsequences ns

setTest :: Integer -> Bool
setTest n = l' == 2 ^ l
            where
                x = set n
                l = length x
                x' = powerSet x
                l' = length x'

-- With n getting larger and larger we'd run in to software limitations eventually. As per the question of what I've tested: I'm checking if the length property of the powerset satisfies my conditions

--Exercise 3 (30 min)


factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * (factorial (n-1))

numberOfOrderingsTest :: Int -> Bool
numberOfOrderingsTest n = l == l'
                                where
                                    x = [1..n]
                                    x' = permutations x
                                    l' = length x'
                                    l = factorial n

-- As with exercise 2: We're hitting software limitations

-- Exercise 4

filterRev :: [Integer] -> [Integer]
filterRev = filter (\ x -> (prime . reversal) x)

prop :: [Integer]
prop = filterRev (take 1000 primes)

-- Exercise 5

consecutive :: Int -> [Integer]
consecutive d = p
   where
    primes' = (take 101 . drop d) primes
    s = prime .sum $ primes'
    p = if s then primes' else consecutive (d + 101)

-- due to the nature of the question and the generation of the list, it is a given to be the smallest prime constructed out of 101 consecutive primes. 

-- Exercise 6 (20 min)

conjecture :: Int -> [Integer]
conjecture d = x
    where
        primes' = take (d + 2) primes
        isPrime = (prime . (+) 1 . product) primes'
        x = if isPrime then conjecture (d+1) else primes'

-- The smallest counter-example is {2,3,5,7,11,13} which summed is 41. Adding one we get an even number, so is not prime

-- Exercise 7 (15 min)

mod10 :: Integral x => x -> x
mod10 x = mod x 10

div10 :: Integral x => x -> x
div10 x = div x 10

convert :: Integral x => x -> [x]
convert 0 = []
convert x = convert (div10 x) ++ [mod10 x]

accountPair :: [x] -> ([x], x)
accountPair x = (init x, last x)

sumDoubleDigit :: Integral x => x -> x
sumDoubleDigit x 
    | x < 10 = x
    | otherwise = x'
        where
            unit = mod x 10
            tens = div x 10
            x' = unit + tens

firstPass :: Integral x => [x] -> [x]
firstPass [] = []
firstPass (x:y:xs) = x : (y * 2) : firstPass xs
firstPass (x:_) = [x]

secondPass :: [Integer] -> [Integer]
secondPass = map(sumDoubleDigit)

thirdPass :: Integral x => [x] -> x
thirdPass = mod10 . (*9) . sum

isValid :: ([Integer], Integer) -> Bool
isValid (a,c) = ((thirdPass . secondPass . firstPass) a) == c

luhn :: Integer -> Bool
luhn = isValid . accountPair . convert  

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = (length (convert n) == 15) && (luhn n)
isMaster n = (length (convert n) == 16) && (luhn n) && (show n !! 0) == '5'
isVisa n = (length (convert n) == 16) && (luhn n) && (show n !! 0) == '4'

-- Exercise 8 (45 min)

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

(!||) :: Bool -> Bool -> Bool
(!||) = xor

accuses :: Boy -> Boy -> Bool
accuses Matthew Jack = True
accuses Matthew Peter = True
accuses Matthew Arnold = True

accuses Peter Matthew = True
accuses Peter Jack = True

accuses Jack x = not ((accuses Matthew x) || (accuses Peter x))

accuses Arnold x = (accuses Matthew x) !|| (accuses Peter x)

accuses Carl x = not (accuses Arnold x)

accuses _ _ = False

accusers :: Boy -> [Boy]
accusers x = filter (\y -> accuses y x) boys

guilty = filter (\x -> length (accusers x ) >= 3) boys
honest = accusers (head guilty)