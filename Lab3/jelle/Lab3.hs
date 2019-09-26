module Lab3 where

import Control.Monad
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture3
import SetOrd

-- Assignment 1: 90 mins

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> if (evl v f1) then (evl v f2) else True) (genVals (propNames f1 ++ propNames f2))

equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1

-- Assignment 2 & 4: 480 mins
-- quickCheck testProp_ (_ = property number) always yields 
-- +++ OK, passed 100 tests.
-- Properties are described in comments above them

genForm :: Int -> Gen Form
genForm 0 = 
    liftM Prop (choose (1,6))
genForm n | n > 0 =
    oneof [liftM Neg subForm,
           liftM Cnj subForms,
           liftM Dsj subForms,
           liftM2 Impl subForm subForm,
           liftM2 Equiv subForm subForm
           ]
    where subForm  = genForm (n `div` 2)
          subForms = vectorOf 2 subForm

sizedForm = sized genForm

-- Same amount of digits in input and output
parseProp0 :: Form -> Bool
parseProp0 f = (countDigits . show) f == ((countDigits . show) (parse (show f)))

testProp0 :: Property
testProp0 = forAll sizedForm parseProp0

-- Order of digit is same in input and output
parseProp1 :: Form -> Bool
parseProp1 f = (digitsList . show) f == (digitsList . show) (parse (show f))

testProp1 :: Property
testProp1 = forAll sizedForm parseProp1

-- Same amount of brackets ( and )
parseProp2 :: Form -> Bool
parseProp2 f = ((countChar . show) f) '(' == (countChar . show) (parse (show f)) '(' &&
                ((countChar . show) f) ')' == (countChar . show) (parse (show f)) ')'

testProp2 :: Property
testProp2 = forAll sizedForm parseProp2

countDigits :: String -> Int
countDigits s = foldl (\a c -> if isDigit c then a + 1 else a) 0 s

digitsList :: String -> [Char]
digitsList s = foldl (\xs c -> if isDigit c then [c] ++ xs else xs) [] s

countChar :: String -> Char -> Int
countChar s char = foldl (\a c -> if c == char then a + 1 else a) 0 s


-- Assignment 4: 120 mins
-- Test with Siwa's code for assignment 3
-- quickCheck testCnf_ (where _ is property number) yields:
-- +++ OK, passed 100 tests.
-- So implementation seems correct.


-- UNCOMMENT CODE & USE CNF IMPLEMENTATION TO TEST

-- Output is arrowfree
-- cnfProp0 :: Form -> Bool
-- cnfProp0 f = not (any (\x -> x == '>' || x == '<') (show (cnf f)))

-- testCnf0 :: Property
-- testCnf0 = forAll sizedForm cnfProp0

-- Input and output are logically equivalent
-- cnfProp1 :: Form -> Bool
-- cnfProp1 f = equiv f (cnf f)

-- testCnf1 :: Property
-- testCnf1 = forAll sizedForm cnfProp0
