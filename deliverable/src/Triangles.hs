{-- Exercise 3
    Tests can be found in ~/test/Spec.hs and ran with `stack test`
--}

module Triangles where
    import Lib
    import Data.List
    import Test.QuickCheck

    isTriangle :: Int -> Int -> Int -> Bool
    isTriangle a b c = let  c' = (a + b) > c
                            b' = (a + c) > b
                            a' = (b + c) > a
                        in  a' && b' && c'

    isEquilateral :: Int -> Int -> Int -> Bool
    isEquilateral a b c = (a == b) && (b == c)

    isIsoceles :: Int -> Int -> Int -> Bool
    isIsoceles a b _ = a == b

    isRectangular :: Int -> Int -> Int -> Bool
    isRectangular a b c = (a ^ 2) + (b ^ 2) == (c ^ 2)

    -- Helper functions

    apply :: [Int] -> (Int -> Int -> Int -> Bool) -> Bool
    apply (x:y:z:_) fn = fn x y z
    apply _ fn = error "Apply needs a list with at least 3 items"

    mapper :: [Int] -> (Int -> Int -> Int -> Bool) -> [Bool]
    mapper xs fn = map (\x -> apply x fn) (permutations xs)

    folder :: [Bool] -> Bool
    folder = foldr (||) False

    apply' xs fn = folder $ mapper xs fn
    
    --- / Helper functions

    triangle :: Int -> Int -> Int -> Shape
    triangle a b c
        | (not . applied) isTriangle = NoTriangle
        | applied isEquilateral = Equilateral
        | appliedPerms isIsoceles = Isosceles
        | appliedPerms isRectangular = Rectangular
        | otherwise = Other
        where
            applied = apply [a,b,c]
            appliedPerms = apply' [a,b,c]

    noZeroesProperty :: Int -> Int -> Int -> Bool
    noZeroesProperty a b c = a > 0 && b > 0 && c > 0
    
    prop_rect a b c  = isTriangle a b c && isRectangular a b c --> triangle a b c == Rectangular
    prop_rect2 a b c = isTriangle a b c && isRectangular a b c --> triangle b c a == Rectangular
    prop_rect3 a b c = isTriangle a b c && isRectangular a b c --> triangle c b a == Rectangular

    prop_isoceles a b c  = isTriangle a b c && isIsoceles a b c --> triangle a b c == Isosceles
    prop_isoceles2 a b c = isTriangle a b c && isIsoceles a b c --> triangle b c a == Isosceles
    prop_isoceles3 a b c = isTriangle a b c && isIsoceles a b c --> triangle c b a == Isosceles

    prop_equilateral a b c = noZeroesProperty a b c && isEquilateral a b c --> triangle a b c == Equilateral
    prop_noTriangle a b c  = noZeroesProperty a b c && (not $ isTriangle a b c) --> triangle a b c == NoTriangle