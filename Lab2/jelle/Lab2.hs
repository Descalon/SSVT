
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Assignment 1 : 75 mins
-- Results: algorithm is probably correct. Consistent percentages around 25 as
-- output.
probsCheck :: Int -> Float -> Float -> IO [Float]
probsCheck n lower upper= do
                  xs <- probs n
                  return (filter (\x -> x <= upper && x >= lower) xs)


probsCheckPercentage :: Int -> Float -> Float -> IO Int
probsCheckPercentage n lower upper = do
                       xs <- probsCheck n lower upper
                       return (((length xs) * 100) `div` n)

-- Call with integer tries as argument to solve problem
probsCheckPercentages :: Int -> IO [Int]
probsCheckPercentages n = do 
                             a <- probsCheckPercentage n 0.0 0.25
                             b <- probsCheckPercentage n 0.25 0.5
                             c <- probsCheckPercentage n 0.5 0.75
                             d <- probsCheckPercentage n 0.75 1.0
                             return [a,b,c,d]

-- Assignment 2: 30 mins
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
    | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2 = Rectangular
    | x == y && y == z = Equilateral
    | x == y || y == z || x == z = Isosceles
    | x + y > z && y + z > x && x + z > y = Other
    | otherwise = NoTriangle

-- Assignment 3: 180 mins
-- run with: assignment3
-- result: prop0 is strongest, then prop1 and prop3 (same strength), then prop2
prop0 :: Int -> Bool
prop0 x = even x && x > 3

prop1 :: Int -> Bool
prop1 x = even x

prop2 :: Int -> Bool
prop2 x = even x || x > 3

prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

props :: [Int -> Bool]
props = [prop0, prop1, prop2, prop3]
propNames :: [String]
propNames = ["prop0", "prop1", "prop2", "prop3"]

propsWithName = zip props propNames

domain :: [Int]
domain = [-10..10]

compare' :: [a] -> (a -> Bool) -> (a -> Bool) -> Int
compare' xs p q = let s = stronger xs p q
                      w = weaker xs p q
                      in if s == w then 0 else if s then 1 else -1

-- Gives a score to a property compared to a list of different properties
score :: [a] -> (a -> Bool) -> ([a -> Bool]) -> Int
score xs p qs = sum (map (\x -> compare' xs p x) qs)

scoreAll input propertyList = map (\ x -> score input x propertyList) propertyList

namesWithScore input l = zip propNames (scoreAll input l)

quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
    where
        lesser = [a | a <- xs, (snd a) < (snd x)]
        greater = [b | b <- xs, (snd b) >= (snd x)]

assignment3 = reverse (quicksort (namesWithScore domain props))

-- assignment 3 (part 2): 

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys = if x `elem` ys then isPermutation xs (delete x ys) else False


-- assignment 4: 60 mins
-- Ordered list of strength: isDeranProp2 is strongest, then Prop1, Prop0 is weakest
-- Testing with quickCheck and Prop0, Prop1 and Prop2 all pass 100 tests.
isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = derangement xs ys

-- Checks whether input list is correct for this assignment ([0..n - 1])
checkList :: [Int] -> Bool
checkList [] = True
checkList xs = xs == [0..((length xs) - 1)]

derangement :: [Int] -> [Int] -> Bool
derangement [] [] = True
derangement [] _ = False
derangement _ [] = False
derangement (x:xs) (y:ys)
        | length (x:xs) /= length (y:ys) = False
        | not (checkList (x:xs))         = False
        | otherwise                      = if x == y then False else derangement xs ys

deran :: [Int] -> [[Int]]
deran xs = filter (\x -> isDerangement x xs) (permutations xs)

isDeranProp0 :: [Int] -> [Int] -> Bool
isDeranProp0 xs ys = (isDerangement xs ys) == ((checkList xs) && (length xs == length ys))

isDeranProp1 :: [Int] -> [Int] -> Bool
isDeranProp1 xs ys = (isDerangement xs ys) == ((checkList xs) && (sort xs == sort ys))

isDeranProp2 :: [Int] -> [Int] -> Bool
isDeranProp2 [] [] = True
isDeranProp2 xs ys 
    | length xs == length ys = isDerangement xs ys == (checkList xs && (all (\x -> x /= (ys !! x)) xs))
    | otherwise              = isDerangement xs ys == False


-- assignment 5: 20 mins
rot13 :: [Char] -> [Char]
rot13 xs = map (\x -> rotChar x) xs

rotChar :: Char -> Char
rotChar char = chr ((((ord char) - 97 + 13) `mod` 26) + 97)

-- assignment 6:
moveToEnd :: [Char] -> [Char]
moveToEnd s = drop 4 (s ++ (take 4 s))

convert :: String -> String
convert [] = []
convert (c:s)
    | c >= 'A' && c <= 'Z' = (replace c) ++ (convert s)
    | otherwise            = c:(convert s)

replace :: Char -> [Char]
replace c = show (ord c - 55)

checkIbanLength :: [Char] -> Bool
checkIbanLength iban = if (length iban) == getIBANLength(take 2 iban) then True else False

iban :: String -> Bool
iban s = if checkIbanLength s then (read (convert (moveToEnd s))::Integer) `mod` 97 == 1 else False

-- Country Code "Dictionary"
getIBANLength :: [Char] -> Int
getIBANLength "AL" = 28
getIBANLength "AD" = 24
getIBANLength "BE" = 16
getIBANLength "BA" = 20
getIBANLength "BG" = 22
getIBANLength "CY" = 28
getIBANLength "DK" = 18
getIBANLength "DE" = 22
getIBANLength "EE" = 20
getIBANLength "FO" = 18
getIBANLength "FI" = 18
getIBANLength "FR" = 27
getIBANLength "GE" = 22
getIBANLength "GI" = 23
getIBANLength "GR" = 27
getIBANLength "GL" = 18
getIBANLength "HU" = 28
getIBANLength "IE" = 22
getIBANLength "IS" = 26
getIBANLength "IL" = 23
getIBANLength "IT" = 27
getIBANLength "JO" = 27
getIBANLength "HR" = 21
getIBANLength "LV" = 21
getIBANLength "LB" = 28
getIBANLength "LI" = 21
getIBANLength "LT" = 20
getIBANLength "LU" = 20
getIBANLength "MK" = 19
getIBANLength "MT" = 31
getIBANLength "MU" = 30
getIBANLength "MC" = 27
getIBANLength "ME" = 22
getIBANLength "NL" = 18
getIBANLength "NO" = 15
getIBANLength "AT" = 20
getIBANLength "PL" = 28
getIBANLength "PT" = 25
getIBANLength "RO" = 24
getIBANLength "SM" = 27
getIBANLength "SA" = 24
getIBANLength "RS" = 22
getIBANLength "SK" = 24
getIBANLength "SI" = 19
getIBANLength "ES" = 24
getIBANLength "CZ" = 24
getIBANLength "TR" = 26
getIBANLength "TN" = 24
getIBANLength "GB" = 22
getIBANLength "AE" = 23
getIBANLength "SE" = 24
getIBANLength "CH" = 21
getIBANLength _ = 0
