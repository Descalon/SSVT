module Lab4 where

import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

import System.Random


infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

type Rel a = [(a,a)]


{--
  Exercise 1: Implement a random data generator for the datatype Set Int
--}

-- Implementation that uses QuickCheck

-- Exercise 4: 90 mins
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


{--
  Exercise 5: Function that gives the transitive closure
  Starttime: 14:24
  Endtime: 15:23
--}
ex1 = [(1,2),(2,3),(3,4)]
ex2 = [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]

trClos :: Ord a => Rel a -> Rel a
trClos xs = traClos' xs (-1)
              where
                traClos' xs c
                  | c == 0 = nub xs
                  | otherwise = traClos' genTransitives ((length(genTransitives)) - (length xs))
                      where
                        genTransitives = xs ++ [ (x, z) | (x, y) <- xs, (w, z) <- xs, y==w, (elem) (x,z) xs == False]


{--
  Exercise 7: Is there a difference between the symmetric closure of the transitive
              closure of a relation R and the transitive closure of the symmetric closure of R?
  Starttime: 15:23
  Endtime:   15:43

  Answer: Yes, there is a difference.
  Proof by Counterexample:
  Hypothesis: There is no difference between the symmetric closure of the transitive
              closure of a relation R and the transitive closure of the symmetric closure of R

  Example: R = [(1,2),(2,3),(3,4)]

  Case 1: Symmetric closure of the transitive closure
  Transitive Clusure: [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
  Symmetric closure of the transitive closure: [(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]

  Case 2: Transitive closure of the Symmetric closure
  Symmetric Closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
  Transitive closure of the symmetric closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,1),(1,3),(2,2),(2,4),(3,1),(3,3),(4,2),(4,4),(1,4),(4,1)]

  Counterexample : [(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)] =/= [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,1),(1,3),(2,2),(2,4),(3,1),(3,3),(4,2),(4,4),(1,4),(4,1)]
--}


















{--}
