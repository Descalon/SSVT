module Lab4 where

import Control.Monad
import Data.List
import System.Random
import SetOrd
import Test.QuickCheck

-- Assignment 1: 60 mins
randomSetInt :: Int -> IO (Set Int)
randomSetInt n = do
    g <- newStdGen
    return (Set ((sort . removeDuplicates) (take n (randomRs (0, 9) g))))

randomSetIntQC :: Int -> Gen (Set Int)
randomSetIntQC n = do
    xs <- (listOf $ elements [0..9])
    return (Set (take n ((sort . removeDuplicates) xs)))

-- Remove duplicates from list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | elem x xs = removeDuplicates xs
    | otherwise = (x:(removeDuplicates xs))

-- Assignment 2: 90 mins
listIntersection :: [Int] -> [Int] -> [Int]
listIntersection [] _ = []
listIntersection _ [] = []
listIntersection (x:xs) ys
    | x `elem` ys = (x:(listIntersection xs ys))
    | otherwise   = listIntersection xs ys
    
setIntersection :: Set Int -> Set Int -> Set Int
setIntersection (Set xs) (Set ys) = Set (listIntersection xs ys)

setUnion :: Set Int -> Set Int -> Set Int
setUnion (Set xs) (Set ys) = Set ((sort . removeDuplicates) (xs ++ ys))

listDifference :: [Int] -> [Int] -> [Int]
listDifference [] _  = []
listDifference xs [] = xs
listDifference (x:xs) ys
    | x `elem` ys = listDifference xs ys
    | otherwise   = (x:(listDifference xs ys))

setDifference :: (Set Int) -> (Set Int) -> (Set Int)
setDifference (Set xs) (Set ys) = Set (sort (listDifference xs ys))

testDiff :: IO (Set Int)
testDiff = do
        let s = sized randomSetIntQC
        g <- generate s
        putStrLn (show g)
        g' <- generate s
        putStrLn (show g')
        return (setDifference g g')

testUnion :: IO (Set Int)
testUnion = do
        let s = sized randomSetIntQC
        g <- generate s
        putStrLn (show g)
        g' <- generate s
        putStrLn (show g')
        return (setUnion g g')

testInter :: IO (Set Int)
testInter = do
        let s = sized randomSetIntQC
        g <- generate s
        putStrLn (show g)
        g' <- generate s
        putStrLn (show g')
        return (setIntersection g g')


-- Assignment 3: 30 mins
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (x:xs) = removeDuplicates (x : reverseTuple x : symClos xs)

reverseTuple :: (a,a) -> (a,a)
reverseTuple (x,y) = (y,x)

-- Assignment 4: 90 mins
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial xs rel = isSerialHelper xs xs rel

isSerialHelper :: Eq a => [a] -> [a] -> Rel a -> Bool
isSerialHelper [] _ _ = True
isSerialHelper (x:xs) ys rel = if checkInRel x ys rel then isSerialHelper xs ys rel else False

checkInRel :: Eq a => a -> [a] -> Rel a -> Bool
checkInRel _ _ [] = False
checkInRel x xs ((y,z):ys)
    | x == y && z `elem` xs = True
    | otherwise             = checkInRel x xs ys

isReflexive :: Ord a => [a] -> Rel a -> Bool
isReflexive [] rel      = True
isReflexive (x:xs) rel = if (x,x) `elem` rel then isReflexive xs rel else False

-- prop :: Ord a => [a] -> Rel a -> Bool
-- prop domain rel = isReflexive domain ((symClos . trClos) rel) && isSerial domain rel

-- Assignment 5: 90 mins
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos rel = sortPairs (trClosHelper ((rel @@ rel) ++ rel) rel)

trClosHelper :: Ord a => Rel a -> Rel a -> Rel a
trClosHelper inputRel outputRel
    | inputRel == outputRel = outputRel
    | otherwise             = trClosHelper (nub ((inputRel @@ inputRel) ++ inputRel)) inputRel

sortPairs :: Ord a => Rel a -> Rel a
sortPairs rel = sortBy comparePairs rel

comparePairs :: Ord a => (a,a) -> (a,a) -> Ordering
comparePairs (a,b) (c,d)
    | a < c     = LT
    | a > c     = GT
    | b < d     = LT
    | b > d     = GT
    | otherwise = EQ
