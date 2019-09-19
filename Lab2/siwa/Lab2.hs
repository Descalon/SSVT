module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-- Red Curry
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

floatList1000 :: IO [Float] -> IO [Int]
floatList1000 iofs = do
                        floats <- iofs
                        let a = length [f | f <- floats, f < 0.25 ]
                        let b = length [f | f <- floats, f >= 0.25, f < 0.5]
                        let c = length [f | f <- floats, f >= 0.5, f < 0.75]
                        let d = length [f | f <- floats, f >= 0.75 ]
                        return ([a,b,c,d])

-- Recognizing triangles
isTriangle :: Int -> Int -> Int -> Shape
isTriangle a b c
    | not ((a + b > c) && (a + c > b) && (b + c > a)) = NoTriangle
    | (a == b && b == c) = Equilateral
    | (a == b || b == c || a == c) && not (a == b && b == c) = Isosceles
    | ((a^2 + b^2 == c^2) || (b^2 + c^2 == a^2) || (a^2 + c^2 == b^2)) = Rectangular
    | otherwise = Other


-- Testing properties strength

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger  xs p q = forall xs (\x -> p x --> q x)
weaker    xs p q = stronger xs q p

{--
a) Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of
type Int -> Bool. Consider a small domain like [(−10)..10].
--}

domain :: [Int]
domain = [(-10)..10]

three1a :: Int->Bool
three1a =  \x -> even x && x > 3

three1b :: Int->Bool
three1b = even

three2a :: Int->Bool
three2a = \x -> even x || x > 3

three2b :: Int->Bool
three2b = even

three3a :: Int->Bool
three3a = \x -> (even x && x > 3) || even x

three3b :: Int->Bool
three3b = even

three4a :: Int->Bool
three4a = even

three4b :: Int->Bool
three4b = \x -> (even x && x > 3) || even x

{--
b) Provide a descending strength list of all the implemented properties.
--}
allproperties :: [((Int->Bool), [Char])]
allproperties = [(three1a, "three1a"),(three1b, "three1b"),(three2a, "three2a"),(three2b, "three2b"),(three3a, "three3a"),(three3b, "three3b"),(three4a, "three4a"),(three4b, "three4b")]

strongSort :: [((Int->Bool), [Char])] -> [[Char]]
strongSort [] = []
strongSort (x:xs) = strongSort strongSortWeaker ++ [snd x] ++ strongSort strongSortStronger
                      where
                        strongSortWeaker = [f | f <- xs, (weaker domain (fst f) (fst x))]
                        strongSortStronger = [f | f <- xs, (stronger domain (fst f) (fst x)) && not (weaker domain (fst f) (fst x)) ]

-- Recognizing Permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (and ([elem x ys | x <- xs] ++ [elem y xs | y <- ys])) && (length [elem x ys | x <- xs] == length [elem y xs | y <- ys])


allDifferent ::  Eq a =>  [a] -> [a] -> Int -> Bool
allDifferent [] ys c = True
allDifferent (x:xs) ys c = if (x == (ys !! c)) then False else
                                  allDifferent xs ys (c+1)

-- Recognizing and generating derangements
isDerangement ::  Eq a => [a] -> [a] -> Bool
isDerangement xs ys   | not (isPermutation xs ys) = False
                      | otherwise = (allDifferent xs ys 0)

deran :: Int -> [[Int]]
deran n = [ x | x <- (permutations [0..(n-1)]), (isDerangement x [0..(n-1)])]

-- Implementing and testing ROT13 encoding
rot13Char :: Char -> Char
rot13Char c   | c >= 'A' && c <= 'Z' = chr((((mod) (((ord c) - (ord 'A')) + 13) 26) + (ord 'A')))
              | c >= 'a' && c <= 'z' = chr((((mod) (((ord c) - (ord 'a')) + 13) 26) + (ord 'a')))
              | otherwise = c

rot13 :: [Char] -> [Char]
rot13 cs = [rot13Char c | c <- cs]


-- Implementing and testing IBAN validation

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

iban :: String -> Bool
iban cs | ((length cs) /= (getIBANLength ([cs !! 0] ++ [cs !! 1]))) = False
        | (mod) (joiner (transformIBAN cs)) 97 == 1 = True
        | otherwise = False

transformIBAN :: String -> [Integer]
transformIBAN (a:b:c:d:es) = [letterToNumber x | x <- (es ++ [a] ++ [b] ++ [c] ++ [d])]

letterToNumber :: Char -> Integer
letterToNumber c = if (c >= 'A' && c <= 'Z') then toInteger ((ord c) - (ord 'A') + 10) else (read [c]::Integer)

joiner :: [Integer] -> Integer
joiner = read . concatMap show


-- Test IBAN verification function
printLines [] = print "done"
printLines (x:xs) = do
                  print (Text.unpack x)
                  print (length (Text.unpack x))
                  let a = iban (Text.unpack x)
                  print a
                  printLines xs

testKnownGoodIBAN = do
    ls <- fmap Text.lines (Text.readFile "ibanknowngood.txt")
    printLines (ls)

testKnownBadIBAN = do
    ls <- fmap Text.lines (Text.readFile "ibanknownbad.txt")
    printLines (ls)
