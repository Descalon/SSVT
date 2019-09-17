{-- Exercise 5
    Tests found in ~/test/Spec.hs. Ran with stack test
--}

module Derangement where
    import Lib
    import Data.List

    isDerangement :: [Int] -> [Int] -> Bool
    isDerangement [] _ = True
    isDerangement _ [] = True
    isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys

    derangements :: [Int] -> [[Int]]
    derangements xs = filter (isDerangement xs) $ permutations xs

    deran :: Int -> [[Int]]
    deran n = derangements xs
                where xs = [0 .. (n - 1)]

    prop_IsOrdered_Ascending :: [Int] -> Bool
    prop_IsOrdered_Ascending [] = True
    prop_IsOrdered_Ascending (x:[]) = True
    prop_IsOrdered_Ascending (x:y:xs) = x <= y && prop_IsOrdered_Ascending (y:xs)

    prop_IsOrdered_Descending :: [Int] -> Bool
    prop_IsOrdered_Descending [] = True
    prop_IsOrdered_Descending (x:[]) = True
    prop_IsOrdered_Descending (x:y:xs) = x >= y && prop_IsOrdered_Descending (y:xs)

    prop_IsOrdered :: [Int] -> Bool
    prop_IsOrdered xs = descending || ascending
                    where
                        descending = prop_IsOrdered_Descending xs
                        ascending = prop_IsOrdered_Ascending xs

    prop_NotOrdered :: [Int] -> Bool
    prop_NotOrdered = not . prop_IsOrdered

    prop_Not_Empty :: [a] -> Bool
    prop_Not_Empty xs = (length xs) > 0

    teststructure pre xs post = pre --> forall d (\x -> post x)
                            where d = derangements xs
    (!!!) = teststructure

    test_Should_Disorder :: [Int] -> Bool
    test_Should_Disorder xs = (!!!) precondition xs postcondition
                            where 
                                precondition = prop_Not_Empty xs && prop_IsOrdered_Ascending xs
                                postcondition = (not . prop_IsOrdered_Ascending)