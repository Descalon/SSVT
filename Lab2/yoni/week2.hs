module Lab2 where
import Data.List
import Data.Function (on)
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- ----------------------------------------------------------------

-- 1 hour+, monads / IO
exercise1 :: IO [Float] -> IO [Int]
exercise1 n = do
            floats <- n
            let a = length [k | k <- floats, k < 0.25]
            let b = length [k | k <- floats, k < 0.5, k >= 0.25]
            let c = length [k | k <- floats, k >= 0.5, k < 0.75]
            let d = length [k | k <- floats, k >= 0.75]
            return ([a, b, c, d])

-- 50min >, exercise 2
exercise2 :: (Integer, Integer, Integer) -> Shape
exercise2 (a, b, c)
    | not ((a + b > c) && (a + c > b) && (b + c > a)) = NoTriangle
    | (a == b && b == c) = Equilateral
    | (a == b || b == c || a == c) && not (a == b && b == c) = Isosceles
    | ((a ^ 2 + b ^ 2 == c ^ 2) || (b ^ 2 + c ^ 2 == a ^ 2) || (a ^ 2 + c ^ 2 == b ^ 2)) = Rectangular
    | otherwise = Other

noTriangles = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], not ((a + b > c) && (a + c > b) && (b + c > a))]
equilaterals = [(a, a, a) | a <- [1..10]]
isosceli = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], (a == b || b == c || a == c) && not (a == b && b == c)]
rectangula = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], ((a ^ 2 + b ^ 2 == c ^ 2) || (b ^ 2 + c ^ 2 == a ^ 2) || (a ^ 2 + c ^ 2 == b ^ 2))]
randomTriples = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10]]

-- exercise 3 1 hour-ish
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- the list that will be used to compare functions
list = [-10..10]

-- string variant
x = [
    "(\\x -> even x && x > 3)",
    "even",
    "(\\x -> even x || x > 3)",
    "(\\x -> (even x && x > 3) || even x)"]

-- the methods
y = [
    (\x -> even x && x > 3),
    even,
    (\x -> even x || x > 3),
    (\x -> (even x && x > 3) || even x)]

-- return 1 when stronger, return -1 when weaker, return 0 when equal (both stronger and weaker)
compare' :: [a] -> (a -> Bool) -> (a -> Bool) -> Int
compare' xs p q = if stronger xs p q && weaker xs p q then 0 else (if stronger xs p q then 1 else (if weaker xs p q then -1 else 0))

-- score from xs methods to methods
score :: [a] -> (a -> Bool) -> ([a -> Bool]) -> Int
score xs p q = sum (map (\x -> compare' xs p x) q)

-- list of scores from methods to methods
scoreAll :: [a] -> ([a -> Bool]) -> [Int]
scoreAll xs fs = map(\x -> score xs x fs) fs

-- order by 2nd tuple var (score Int)
sortBySnd = sortBy (flip compare `on` snd)

-- sort the whole list, but use the string variant in the zip
sortedScoreList = sortBySnd (zip x (scoreAll list y))

-- exercise 4 one and a half hour, an hour extra for testing and such (currying)
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (and ([elem x ys | x <- xs] ++ [elem y xs | y <- ys])) && (length [elem x ys | x <- xs] == length [elem y xs | y <- ys])

{-- 
    Q: You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
    A: Given the method will not be tested on this case it may result in the method being too strict in the end since 
       the initial requirement was "possibly" in a different order.
--}

permPropOne :: Eq a => [a] -> [a] -> Bool
permPropOne xs ys = (and ([elem x ys | x <- xs] ++ [elem y xs | y <- ys]))

permPropTwo :: Eq a => [a] -> [a] -> Bool
permPropTwo xs ys = (length [elem x ys | x <- xs] == length [elem y xs | y <- ys])

-- used for testing score
permPropThree :: Eq a => [a] -> [a] -> Bool
permPropThree _ _= False

permList = [1..5] -- test list
permList2 = reverse permList -- succes
permList3 = drop 2 permList -- fail?
permList4 = [5,3,2,1,4] -- sucess
permList5 = [] -- fail
permList6 = permList ++ [1] -- fail
permList7 = map(\x -> x + 1) permList

permTest1 = isPermutation permList permList  -- succes
permTest2 = isPermutation permList permList2 -- succes
permTest3 = isPermutation permList permList3 -- fail
permTest4 = isPermutation permList permList4 -- succes
permTest5 = isPermutation permList permList5 -- fail
permTest6 = isPermutation permList permList6 -- fail
permTest7 = isPermutation permList permList7 -- fail

-- ugly, yup
allPermLists = [permList, permList2, permList3, permList4, permList5, permList6, permList7]

-- outputs 0, based on permList scenarios the properties are equal in strength
-- in certain scenarios one can be stronger than the other and vice versa
permScore p q = sum $ map (\x -> compare' xs (p x) (q x)) xs
                where
                    xs = allPermLists

permTest8 :: Int -> Bool
permTest8 n = isPermutation [1..n] (reverse [1..n])
quickTest1 = quickCheckResult permTest8

-- Another good quick test would be shuffling the 2nd permList, although simply reversing covers quite a lot on itself