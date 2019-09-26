import SetOrd
import System.Random
import Test.QuickCheck
import Data.List

getRandInt :: Int -> IO (Set Int)
getRandInt n = do 
    g <- newStdGen
    return( Set (take n (randomRs(1, 9) g)))


-- assignment 3 - an hour
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos xs =  uniq $ ssymclose xs -- make sure we don't have duplicates

ssymclose :: Ord a => Rel a -> Rel a
ssymclose [] = []
ssymclose (x:xs) = x : reverseRel x : symClos xs

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = (if x `elem` xs then id else (x:)) $ uniq xs

reverseRel :: Ord a => (a, a) -> (a, a)
reverseRel (x, y) = (y, x)


-- assignment 4
{--
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial (x:xs) a = if (any(\rel -> fst rel == x) a) then isSerial xs a else False
isSerial x a = if (any(\rel -> fst rel == x) a) then True else False
-- check whether any a existis in Rel a such that; (a, smth)
--}
-- an hour or so, were considering wether [a] would be the exact same domain as the one of Rel a, which it was
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial xs r = all (\x -> elem x r') xs
    where
        r' = map (fst) r


-- 5?
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos rel = sortPairs (trClosHelper ((rel @@ rel) ++ rel) rel)

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

findReflexive :: Eq a => a -> Rel a -> Rel a
findReflexive x rs = filter (\(f,s) -> x == f && x == s) rs

isReflexive :: Eq a => [a] -> Rel a -> Bool
isReflexive domain rs = all (\x -> case findReflexive x rs of [] -> False; _ -> True) domain

reflexiveGenerator :: [a] -> Rel a
reflexiveGenerator domain = [(x,x) | x <- domain]

-- assignment 6
-- symClos
inverseRel :: Ord a => Rel a -> Rel a
inverseRel [] = []
inverseRel (x:xs) = reverseRel x : inverseRel xs

-- valid sym test
prop_sym_inverse_equals :: Ord a => Rel a -> Bool
prop_sym_inverse_equals r = all(\x -> x `elem` iSc) sC && all(\x -> x `elem` sC) iSc
        where 
            sC = symClos r
            iSc = inverseRel sC

-- Valid trans test
prop_sym_tr_is_reflexive :: Ord a => [a] -> Rel a -> Bool
prop_sym_tr_is_reflexive d r = isReflexive d (trClos $ symClos r)


-- This is on rel sets rather than rels therefore invalid, although, it should somehow be possible
-- prop_trans_intersect_is_trans :: Ord a => Rel a -> Rel a -> Rel a
-- prop_trans_intersect_is_trans xs ys = filter(\x -> x `elem` ys) xs