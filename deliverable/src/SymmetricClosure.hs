module SymmetricClosure where
    import Data.List

    -- assignment 3 - an hour
    type Rel a = [(a,a)]

    symClos :: Ord a => Rel a -> Rel a
    symClos [] = []
    symClos xs =  nub $ xs ++ xs'
        where xs' = reverseRel xs

    reverseRel :: Ord a => Rel a -> Rel a
    reverseRel r = [(y,x) | (x,y) <- r]