module TransitiveClosure where
    import Lib
    import Data.List

    type Rel a = [(a,a)]

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