module SymmetricClosure where

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